{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Operators on 'Column's.  Please note that numeric 'Column' types
-- are instances of 'Num', so you can use '*', '/', '+', '-' on them.

module Opaleye.Operators (module Opaleye.Operators) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NEL

import           Opaleye.Internal.Column (Column(Column), unsafeCase_,
                                          unsafeIfThenElse, unsafeGt)
import qualified Opaleye.Internal.Column as C
import           Opaleye.Internal.QueryArr (QueryArr(QueryArr), Query, runSimpleQueryArr)
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Operators as O
import           Opaleye.Internal.Helpers   ((.:))
import qualified Opaleye.Order as Ord
import qualified Opaleye.Select   as S
import qualified Opaleye.SqlTypes as T

import qualified Opaleye.Column   as Column
import qualified Opaleye.Distinct as Distinct
import qualified Opaleye.Join     as Join

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

-- ^ We can probably disable ConstraintKinds and TypeSynonymInstances
-- when we move to Sql... instead of PG..

-- * Restriction operators

{-| Keep only the rows of a query satisfying a given condition, using
an SQL @WHERE@ clause.

You would typically use 'restrict' if you want to write your query
using 'A.Arrow' notation.  If you want to use a "point free" style
then 'keepWhen' will suit you better.

(If you are familiar with 'Control.Monad.MonadPlus' or
'Control.Applicative.Alternative' it may help you to know that
'restrict' corresponds to the 'Control.Monad.guard' function.) -}
restrict :: S.SelectArr (Column T.SqlBool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.restrict predicate primQ, t0)

{-| Add a @WHERE EXISTS@ clause to the current query. -}
restrictExists :: S.SelectArr a b -> S.SelectArr a ()
restrictExists criteria = QueryArr f where
  f (a, primQ, t0) = ((), PQ.exists primQ existsQ, t1) where
    (_, existsQ, t1) = runSimpleQueryArr criteria (a, t0)

{-| Add a @WHERE NOT EXISTS@ clause to the current query. -}
restrictNotExists :: S.SelectArr a b -> S.SelectArr a ()
restrictNotExists criteria = QueryArr f where
  f (a, primQ, t0) = ((), PQ.notExists primQ existsQ, t1) where
    (_, existsQ, t1) = runSimpleQueryArr criteria (a, t0)

{-| Keep only the rows of a query satisfying a given condition, using
an SQL @WHERE@ clause.

You would typically use 'keepWhen' if you want to write
your query using a "point free" style.  If you want to use 'A.Arrow'
notation then 'restrict' will suit you better.

This is the 'S.SelectArr' equivalent of 'Prelude.filter' from the
'Prelude'.
-}
keepWhen :: (a -> Column T.SqlBool) -> S.SelectArr a a
keepWhen p = proc a -> do
  restrict  -< p a
  A.returnA -< a

-- * Equality operators

infix 4 .==
(.==) :: Column a -> Column a -> Column T.SqlBool
(.==) = C.binOp (HPQ.:==)

infix 4 ./=
(./=) :: Column a -> Column a -> Column T.SqlBool
(./=) = C.binOp (HPQ.:<>)

infix 4 .===
-- | A polymorphic equality operator that works for all types that you
-- have run `makeAdaptorAndInstance` on.  This may be unified with
-- `.==` in a future version.
(.===) :: D.Default O.EqPP columns columns => columns -> columns -> Column T.SqlBool
(.===) = (O..==)

infix 4 ./==
-- | A polymorphic inequality operator that works for all types that
-- you have run `makeAdaptorAndInstance` on.  This may be unified with
-- `./=` in a future version.
(./==) :: D.Default O.EqPP columns columns => columns -> columns -> Column T.SqlBool
(./==) = Opaleye.Operators.not .: (O..==)

-- * Comparison operators

infix 4 .>
(.>) :: Ord.SqlOrd a => Column a -> Column a -> Column T.SqlBool
(.>) = unsafeGt

infix 4 .<
(.<) :: Ord.SqlOrd a => Column a -> Column a -> Column T.SqlBool
(.<) = C.binOp (HPQ.:<)

infix 4 .<=
(.<=) :: Ord.SqlOrd a => Column a -> Column a -> Column T.SqlBool
(.<=) = C.binOp (HPQ.:<=)

infix 4 .>=
(.>=) :: Ord.SqlOrd a => Column a -> Column a -> Column T.SqlBool
(.>=) = C.binOp (HPQ.:>=)

-- * Numerical operators

-- | Integral division, named after 'Prelude.quot'.  It maps to the
-- @/@ operator in Postgres.
quot_ :: C.SqlIntegral a => Column a -> Column a -> Column a
quot_ = C.binOp (HPQ.:/)

-- | The remainder of integral division, named after 'Prelude.rem'.
-- It maps to 'MOD' ('%') in Postgres, confusingly described as
-- "modulo (remainder)".
rem_ :: C.SqlIntegral a => Column a -> Column a -> Column a
rem_ = C.binOp HPQ.OpMod

-- * Conditional operators

-- | Select the first case for which the condition is true.
case_ :: [(Column T.SqlBool, Column a)] -> Column a -> Column a
case_ = unsafeCase_

-- | Monomorphic if\/then\/else.
--
-- This may be replaced by 'ifThenElseMany' in a future version.
ifThenElse :: Column T.SqlBool -> Column a -> Column a -> Column a
ifThenElse = unsafeIfThenElse

-- | Polymorphic if\/then\/else.
ifThenElseMany :: D.Default O.IfPP columns columns
               => Column T.SqlBool
               -> columns
               -> columns
               -> columns
ifThenElseMany = O.ifExplict D.def

-- * Logical operators

infixr 2 .||

-- | Boolean or
(.||) :: Column T.SqlBool -> Column T.SqlBool -> Column T.SqlBool
(.||) = C.binOp HPQ.OpOr

infixr 3 .&&

-- | Boolean and
(.&&) :: Column T.SqlBool -> Column T.SqlBool -> Column T.SqlBool
(.&&) = (O..&&)

-- | Boolean not
not :: Column T.SqlBool -> Column T.SqlBool
not = C.unOp HPQ.OpNot

-- | True when any element of the container is true
ors :: F.Foldable f => f (Column T.SqlBool) -> Column T.SqlBool
ors = F.foldl' (.||) (T.sqlBool False)

-- * Text operators

-- | Concatenate 'Column' 'T.SqlText'
(.++) :: Column T.SqlText -> Column T.SqlText -> Column T.SqlText
(.++) = C.binOp (HPQ.:||)

-- | To lowercase
lower :: Column T.SqlText -> Column T.SqlText
lower = C.unOp HPQ.OpLower

-- | To uppercase
upper :: Column T.SqlText -> Column T.SqlText
upper = C.unOp HPQ.OpUpper

-- | Postgres @LIKE@ operator
like :: Column T.SqlText -> Column T.SqlText -> Column T.SqlBool
like = C.binOp HPQ.OpLike

-- | Postgres @ILIKE@ operator
ilike :: Column T.SqlText -> Column T.SqlText -> Column T.SqlBool
ilike = C.binOp HPQ.OpILike

charLength :: C.PGString a => Column a -> Column Int
charLength (Column e) = Column (HPQ.FunExpr "char_length" [e])

-- * Containment operators

-- | 'in_' is designed to be used in prefix form.
--
-- 'in_' @validProducts@ @product@ checks whether @product@ is a valid
-- product.  'in_' @validProducts@ is a function which checks whether
-- a product is a valid product.
in_ :: (Functor f, F.Foldable f) => f (Column a) -> Column a -> Column T.SqlBool
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
        => columns -> S.Select columns -> S.Select (Column T.SqlBool)
inQuery c q = qj'
  where -- Remove every row that isn't equal to c
        -- Replace the ones that are with '1'
        q' = A.arr (const 1)
             A.<<< keepWhen (c .===)
             A.<<< q

        -- Left join with a query that has a single row
        -- We either get a single row with '1'
        -- or a single row with 'NULL'
        qj :: Query (Column T.SqlInt4, Column (C.Nullable T.SqlInt4))
        qj = Join.leftJoin (A.arr (const 1))
                           (Distinct.distinct q')
                           (uncurry (.==))

        -- Check whether it is 'NULL'
        qj' :: Query (Column T.SqlBool)
        qj' = A.arr (Opaleye.Operators.not
                     . Column.isNull
                     . snd)
              A.<<< qj

-- * JSON operators

-- | Class of Postgres types that represent json values.
-- Used to overload functions and operators that work on both 'T.SqlJson' and 'T.SqlJsonb'.
--
-- Warning: making additional instances of this class can lead to broken code!
class PGIsJson a

type SqlIsJson = PGIsJson

instance PGIsJson T.SqlJson
instance PGIsJson T.SqlJsonb

-- | Class of Postgres types that can be used to index json values.
--
-- Warning: making additional instances of this class can lead to broken code!
class PGJsonIndex a

type SqlJsonIndex = PGJsonIndex

instance PGJsonIndex T.SqlInt4
instance PGJsonIndex T.SqlInt8
instance PGJsonIndex T.SqlText

-- | Get JSON object field by key.
infixl 8 .->
(.->) :: (SqlIsJson a, SqlJsonIndex k)
      => Column (C.Nullable a) -- ^
      -> Column k -- ^ key or index
      -> Column (C.Nullable a)
(.->) = C.binOp (HPQ.:->)

-- | Get JSON object field as text.
infixl 8 .->>
(.->>) :: (SqlIsJson a, SqlJsonIndex k)
       => Column (C.Nullable a) -- ^
       -> Column k -- ^ key or index
       -> Column (C.Nullable T.SqlText)
(.->>) = C.binOp (HPQ.:->>)

-- | Get JSON object at specified path.
infixl 8 .#>
(.#>) :: (SqlIsJson a)
      => Column (C.Nullable a) -- ^
      -> Column (T.SqlArray T.SqlText) -- ^ path
      -> Column (C.Nullable a)
(.#>) = C.binOp (HPQ.:#>)

-- | Get JSON object at specified path as text.
infixl 8 .#>>
(.#>>) :: (SqlIsJson a)
       => Column (C.Nullable a) -- ^
       -> Column (T.SqlArray T.SqlText) -- ^ path
       -> Column (C.Nullable T.SqlText)
(.#>>) = C.binOp (HPQ.:#>>)

-- | Does the left JSON value contain within it the right value?
infix 4 .@>
(.@>) :: Column T.SqlJsonb -> Column T.SqlJsonb -> Column T.SqlBool
(.@>) = C.binOp (HPQ.:@>)

-- | Is the left JSON value contained within the right value?
infix 4 .<@
(.<@) :: Column T.SqlJsonb -> Column T.SqlJsonb -> Column T.SqlBool
(.<@) = C.binOp (HPQ.:<@)

-- | Does the key/element string exist within the JSON value?
infix 4 .?
(.?) :: Column T.SqlJsonb -> Column T.SqlText -> Column T.SqlBool
(.?) = C.binOp (HPQ.:?)

-- | Do any of these key/element strings exist?
infix 4 .?|
(.?|) :: Column T.SqlJsonb -> Column (T.SqlArray T.SqlText) -> Column T.SqlBool
(.?|) = C.binOp (HPQ.:?|)

-- | Do all of these key/element strings exist?
infix 4 .?&
(.?&) :: Column T.SqlJsonb -> Column (T.SqlArray T.SqlText) -> Column T.SqlBool
(.?&) = C.binOp (HPQ.:?&)

-- * SqlArray operators

emptyArray :: T.IsSqlType a => Column (T.SqlArray a)
emptyArray = T.sqlArray id []

arrayPrepend :: Column a -> Column (T.SqlArray a) -> Column (T.SqlArray a)
arrayPrepend (Column e) (Column es) = Column (HPQ.FunExpr "array_prepend" [e, es])

singletonArray :: T.IsSqlType a => Column a -> Column (T.SqlArray a)
singletonArray x = arrayPrepend x emptyArray

index :: (C.SqlIntegral n) => Column (T.SqlArray a) -> Column n -> Column (C.Nullable a)
index (Column a) (Column b) = Column (HPQ.ArrayIndex a b)

-- * Range operators

overlap :: Column (T.SqlRange a) -> Column (T.SqlRange a) -> Column T.SqlBool
overlap = C.binOp (HPQ.:&&)

liesWithin :: T.IsRangeType a => Column a -> Column (T.SqlRange a) -> Column T.SqlBool
liesWithin = C.binOp (HPQ.:<@)

infix 4 .<<
(.<<) :: Column (T.SqlRange a) -> Column (T.SqlRange a) -> Column T.SqlBool
(.<<) = C.binOp (HPQ.:<<)

infix 4 .>>
(.>>) :: Column (T.SqlRange a) -> Column (T.SqlRange a) -> Column T.SqlBool
(.>>) = C.binOp (HPQ.:>>)

infix 4 .&<
(.&<) :: Column (T.SqlRange a) -> Column (T.SqlRange a) -> Column T.SqlBool
(.&<) = C.binOp (HPQ.:&<)

infix 4 .&>
(.&>) :: Column (T.SqlRange a) -> Column (T.SqlRange a) -> Column T.SqlBool
(.&>) = C.binOp (HPQ.:&>)

infix 4 .-|-
(.-|-) :: Column (T.SqlRange a) -> Column (T.SqlRange a) -> Column T.SqlBool
(.-|-) = C.binOp (HPQ.:-|-)

-- * Other operators

timestamptzAtTimeZone :: Column T.SqlTimestamptz
                      -> Column T.SqlText
                      -> Column T.SqlTimestamp
timestamptzAtTimeZone = C.binOp HPQ.OpAtTimeZone

-- * Deprecated

{-# DEPRECATED doubleOfInt
    "Use 'C.unsafeCast' instead. \
    \Will be removed in version 0.7." #-}
doubleOfInt :: Column T.SqlInt4 -> Column T.SqlFloat8
doubleOfInt (Column e) = Column (HPQ.CastExpr "float8" e)

-- | Identical to 'restrictExists'.  Will be deprecated in version 0.7.
exists :: QueryArr a b -> QueryArr a ()
exists = restrictExists

-- | Identical to 'restrictNotExists'.  Will be deprecated in version 0.7.
notExists :: QueryArr a b -> QueryArr a ()
notExists = restrictNotExists
