{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Operators on 'Column's.  Please note that numeric 'Column' types
-- are instances of 'Num', so you can use '*', '/', '+', '-' on them.

module Opaleye.Operators (module Opaleye.Operators,
                          (O..&&)) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NEL

import           Opaleye.Internal.Column (Column(Column), unsafeCase_,
                                          unsafeIfThenElse, unsafeGt)
import qualified Opaleye.Internal.Column as C
import           Opaleye.Internal.QueryArr (QueryArr(QueryArr), Query)
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Operators as O
import           Opaleye.Internal.Helpers   ((.:))
import qualified Opaleye.Order as Ord
import qualified Opaleye.PGTypes as T

import qualified Opaleye.Column   as Column
import qualified Opaleye.Distinct as Distinct
import qualified Opaleye.Join     as Join

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

{-| Restrict query results to a particular condition.  Corresponds to
the guard method of the MonadPlus class.  You would typically use
'restrict' if you want to use 'A.Arrow' notation.  -}
restrict :: QueryArr (Column T.PGBool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.restrict predicate primQ, t0)

{-| Filter a 'QueryArr' to only those rows where the given condition
holds.  This is the 'QueryArr' equivalent of 'Prelude.filter' from the
'Prelude'.  You would typically use 'keepWhen' if you want to use a
\"point free\" style.-}
keepWhen :: (a -> Column T.PGBool) -> QueryArr a a
keepWhen p = proc a -> do
  restrict  -< p a
  A.returnA -< a

infix 4 .==
(.==) :: Column a -> Column a -> Column T.PGBool
(.==) = C.binOp (HPQ.:==)

infix 4 ./=
(./=) :: Column a -> Column a -> Column T.PGBool
(./=) = C.binOp (HPQ.:<>)

infix 4 .===
-- | A polymorphic equality operator that works for all types that you
-- have run `makeAdaptorAndInstance` on.  This may be unified with
-- `.==` in a future version.
(.===) :: D.Default O.EqPP columns columns => columns -> columns -> Column T.PGBool
(.===) = (O..==)

infix 4 ./==
-- | A polymorphic inequality operator that works for all types that
-- you have run `makeAdaptorAndInstance` on.  This may be unified with
-- `./=` in a future version.
(./==) :: D.Default O.EqPP columns columns => columns -> columns -> Column T.PGBool
(./==) = Opaleye.Operators.not .: (O..==)

infix 4 .>
(.>) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.>) = unsafeGt

infix 4 .<
(.<) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.<) = C.binOp (HPQ.:<)

infix 4 .<=
(.<=) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.<=) = C.binOp (HPQ.:<=)

infix 4 .>=
(.>=) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.>=) = C.binOp (HPQ.:>=)

quot_ :: C.PGIntegral a => Column a -> Column a -> Column a
quot_ = C.binOp (HPQ.:/)

rem_ :: C.PGIntegral a => Column a -> Column a -> Column a
rem_ = C.binOp HPQ.OpMod

case_ :: [(Column T.PGBool, Column a)] -> Column a -> Column a
case_ = unsafeCase_

-- | Monomorphic if\/then\/else.
--
-- This may be replaced by 'ifThenElseMany' in a future version.
ifThenElse :: Column T.PGBool -> Column a -> Column a -> Column a
ifThenElse = unsafeIfThenElse

-- | Polymorphic if\/then\/else.
ifThenElseMany :: D.Default O.IfPP columns columns
               => Column T.PGBool
               -> columns
               -> columns
               -> columns
ifThenElseMany = O.ifExplict D.def

infixr 2 .||

-- | Boolean or
(.||) :: Column T.PGBool -> Column T.PGBool -> Column T.PGBool
(.||) = C.binOp HPQ.OpOr

not :: Column T.PGBool -> Column T.PGBool
not = C.unOp HPQ.OpNot

-- | Concatenate 'Column' 'T.PGText'
(.++) :: Column T.PGText -> Column T.PGText -> Column T.PGText
(.++) = C.binOp (HPQ.:||)

-- | To lowercase
lower :: Column T.PGText -> Column T.PGText
lower = C.unOp HPQ.OpLower

-- | To uppercase
upper :: Column T.PGText -> Column T.PGText
upper = C.unOp HPQ.OpUpper

-- | Postgres @LIKE@ operator
like :: Column T.PGText -> Column T.PGText -> Column T.PGBool
like = C.binOp HPQ.OpLike

-- | Postgres @ILIKE@ operator
ilike :: Column T.PGText -> Column T.PGText -> Column T.PGBool
ilike = C.binOp HPQ.OpILike

charLength :: C.PGString a => Column a -> Column Int
charLength (Column e) = Column (HPQ.FunExpr "char_length" [e])

-- | True when any element of the container is true
ors :: F.Foldable f => f (Column T.PGBool) -> Column T.PGBool
ors = F.foldl' (.||) (T.pgBool False)

-- | 'in_' is designed to be used in prefix form.
--
-- 'in_' @validProducts@ @product@ checks whether @product@ is a valid
-- product.  'in_' @validProducts@ is a function which checks whether
-- a product is a valid product.
in_ :: (Functor f, F.Foldable f) => f (Column a) -> Column a -> Column T.PGBool
in_ fcas (Column a) = Column $ case NEL.nonEmpty (F.toList fcas) of
   Nothing -> HPQ.ConstExpr (HPQ.BoolLit False)
   Just xs -> HPQ.BinExpr HPQ.OpIn a (HPQ.ListExpr (fmap C.unColumn xs))

-- | True if the first argument occurs amongst the rows of the second,
-- false otherwise.
--
-- This operation is equivalent to Postgres's @IN@ operator but, for
-- expediency, is currently implemented using a @LEFT JOIN@.  Please
-- file a bug if this causes any issues in practice.
inQuery :: D.Default O.EqPP columns columns
        => columns -> QueryArr () columns -> Query (Column T.PGBool)
inQuery c q = qj'
  where -- Remove every row that isn't equal to c
        -- Replace the ones that are with '1'
        q' = A.arr (const 1)
             A.<<< keepWhen (c .===)
             A.<<< q

        -- Left join with a query that has a single row
        -- We either get a single row with '1'
        -- or a single row with 'NULL'
        qj :: Query (Column T.PGInt4, Column (C.Nullable T.PGInt4))
        qj = Join.leftJoin (A.arr (const 1))
                           (Distinct.distinct q')
                           (uncurry (.==))

        -- Check whether it is 'NULL'
        qj' :: Query (Column T.PGBool)
        qj' = A.arr (Opaleye.Operators.not
                     . Column.isNull
                     . snd)
              A.<<< qj

timestamptzAtTimeZone :: Column T.PGTimestamptz
                      -> Column T.PGText
                      -> Column T.PGTimestamp
timestamptzAtTimeZone = C.binOp HPQ.OpAtTimeZone

emptyArray :: T.IsSqlType a => Column (T.PGArray a)
emptyArray = T.pgArray id []

arrayPrepend :: Column a -> Column (T.PGArray a) -> Column (T.PGArray a)
arrayPrepend (Column e) (Column es) = Column (HPQ.FunExpr "array_prepend" [e, es])

singletonArray :: T.IsSqlType a => Column a -> Column (T.PGArray a)
singletonArray x = arrayPrepend x emptyArray

-- | Class of Postgres types that represent json values.
--
-- Used to overload functions and operators that work on both 'T.PGJson' and 'T.PGJsonb'.
--
-- Warning: making additional instances of this class can lead to broken code!
class PGIsJson a

instance PGIsJson T.PGJson
instance PGIsJson T.PGJsonb

-- | Class of Postgres types that can be used to index json values.
--
-- Warning: making additional instances of this class can lead to broken code!
class PGJsonIndex a

instance PGJsonIndex T.PGInt4
instance PGJsonIndex T.PGInt8
instance PGJsonIndex T.PGText

-- | Get JSON object field by key.
infixl 8 .->
(.->) :: (PGIsJson a, PGJsonIndex k)
      => Column (C.Nullable a) -- ^
      -> Column k -- ^ key or index
      -> Column (C.Nullable a)
(.->) = C.binOp (HPQ.:->)

-- | Get JSON object field as text.
infixl 8 .->>
(.->>) :: (PGIsJson a, PGJsonIndex k)
       => Column (C.Nullable a) -- ^
       -> Column k -- ^ key or index
       -> Column (C.Nullable T.PGText)
(.->>) = C.binOp (HPQ.:->>)

-- | Get JSON object at specified path.
infixl 8 .#>
(.#>) :: (PGIsJson a)
      => Column (C.Nullable a) -- ^
      -> Column (T.PGArray T.PGText) -- ^ path
      -> Column (C.Nullable a)
(.#>) = C.binOp (HPQ.:#>)

-- | Get JSON object at specified path as text.
infixl 8 .#>>
(.#>>) :: (PGIsJson a)
       => Column (C.Nullable a) -- ^
       -> Column (T.PGArray T.PGText) -- ^ path
       -> Column (C.Nullable T.PGText)
(.#>>) = C.binOp (HPQ.:#>>)

-- | Does the left JSON value contain within it the right value?
infix 4 .@>
(.@>) :: Column T.PGJsonb -> Column T.PGJsonb -> Column T.PGBool
(.@>) = C.binOp (HPQ.:@>)

-- | Is the left JSON value contained within the right value?
infix 4 .<@
(.<@) :: Column T.PGJsonb -> Column T.PGJsonb -> Column T.PGBool
(.<@) = C.binOp (HPQ.:<@)

-- | Does the key/element string exist within the JSON value?
infix 4 .?
(.?) :: Column T.PGJsonb -> Column T.PGText -> Column T.PGBool
(.?) = C.binOp (HPQ.:?)

-- | Do any of these key/element strings exist?
infix 4 .?|
(.?|) :: Column T.PGJsonb -> Column (T.PGArray T.PGText) -> Column T.PGBool
(.?|) = C.binOp (HPQ.:?|)

-- | Do all of these key/element strings exist?
infix 4 .?&
(.?&) :: Column T.PGJsonb -> Column (T.PGArray T.PGText) -> Column T.PGBool
(.?&) = C.binOp (HPQ.:?&)

-- | Cast a 'PGInt4' to a 'PGFloat8'
doubleOfInt :: Column T.PGInt4 -> Column T.PGFloat8
doubleOfInt (Column e) = Column (HPQ.CastExpr "float8" e)
