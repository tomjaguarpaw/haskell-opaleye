{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

-- We can probably disable ConstraintKinds and TypeSynonymInstances
-- when we move to Sql... instead of PG..

module Opaleye.Operators
  (
  -- * Restriction operators
    where_
  , restrict
  , restrictExists
  , restrictNotExists
  -- * Numerical operators
  -- | Numeric 'Column' / 'F.Field' types are instances of 'Num'
  -- and 'Fractional', so you can use the standard Haskell numerical
  -- operators (e.g.. '*', '/', '+', '-') on them and you can create
  -- them with numerical literals such as @3.14 :: 'F.Field' 'T.SqlFloat8'@.
  , (+)
  , (-)
  , (*)
  , (/)
  , fromInteger
  , abs
  , negate
  , signum
  -- * Equality operators
  , (.==)
  , (./=)
  , (.===)
  , (./==)
  -- * Comparison operators
  , (.>)
  , (.<)
  , (.<=)
  , (.>=)
  -- * Numerical operators
  , quot_
  , rem_
  -- * Conditional operators
  , case_
  , ifThenElse
  , ifThenElseMany
  -- * Logical operators
  , (.||)
  , (.&&)
  , not
  , ors
  -- * Text operators
  , (.++)
  , lower
  , upper
  , like
  , ilike
  , sqlLength
  -- * Containment operators
  , in_
  , inSelect
  -- * JSON operators
  , SqlIsJson
  , SqlJsonIndex
  , PGJsonIndex
  , (.->)
  , (.->>)
  , (.#>)
  , (.#>>)
  , (.@>)
  , (.<@)
  , (.?)
  , (.?|)
  , (.?&)
  , JBOF.jsonBuildObject
  , JBOF.jsonBuildObjectField
  , JBOF.JSONBuildObjectFields
  -- * SqlArray operators
  , emptyArray
  , arrayAppend
  , arrayPrepend
  , arrayRemove
  , arrayRemoveNulls
  , singletonArray
  , index
  , arrayPosition
  , sqlElem
  -- * Range operators
  , overlap
  , liesWithin
  , upperBound
  , lowerBound
  , (.<<)
  , (.>>)
  , (.&<)
  , (.&>)
  , (.-|-)
  -- * Other operators
  , timestamptzAtTimeZone
  , dateOfTimestamp
  , now
  , IntervalNum
  , addInterval
  , minusInterval
  , TimestampPrecision(..)
  , dateTruncTimestamp
  , dateTruncTimestamptz
  -- * Deprecated
  )

  where

import qualified Control.Arrow as A
import qualified Data.Foldable as F hiding (null)
import qualified Data.List.NonEmpty as NEL
import           Prelude hiding (not)
import qualified Opaleye.Exists as E
import qualified Opaleye.Field as F
import           Opaleye.Internal.Column (Field_(Column), Field, FieldNullable,
                                          Nullability(Nullable),
                                          unsafeCase_,
                                          unsafeIfThenElse, unsafeGt)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.JSONBuildObjectFields as JBOF
import           Opaleye.Internal.QueryArr (SelectArr(QueryArr),
                                            runSimpleQueryArr')
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Operators as O
import           Opaleye.Internal.Helpers   ((.:))
import qualified Opaleye.Lateral as L
import qualified Opaleye.Order as Ord
import qualified Opaleye.Select   as S
import qualified Opaleye.SqlTypes as T

import qualified Opaleye.Column   as Column

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

{-| Keep only the rows of a query satisfying a given condition, using an
SQL @WHERE@ clause.  It is equivalent to the Haskell function

@
where_ :: Bool -> [()]
where_ True  = [()]
where_ False = []
@
-}
where_ :: F.Field T.SqlBool -> S.Select ()
where_ = L.viaLateral restrict

{-| You would typically use 'restrict' if you want to write your query
using 'A.Arrow' notation.  If you want to use monadic style
then 'where_' will suit you better. -}
restrict :: S.SelectArr (F.Field T.SqlBool) ()
restrict = O.restrict

{-| Add a @WHERE EXISTS@ clause to the current query. -}
restrictExists :: S.SelectArr a b -> S.SelectArr a ()
restrictExists criteria = QueryArr f where
  -- A where exists clause can always refer to columns defined by the
  -- query it references so needs no special treatment on LATERAL.
  f a = do
    (_, existsQ) <- runSimpleQueryArr' criteria a
    pure ((), PQ.aSemijoin PQ.Semi existsQ)

{-| Add a @WHERE NOT EXISTS@ clause to the current query. -}
restrictNotExists :: S.SelectArr a b -> S.SelectArr a ()
restrictNotExists criteria = QueryArr f where
  -- A where exists clause can always refer to columns defined by the
  -- query it references so needs no special treatment on LATERAL.
  f a = do
    (_, existsQ) <- runSimpleQueryArr' criteria a
    pure ((), PQ.aSemijoin PQ.Anti existsQ)

infix 4 .==
(.==) :: Field a -> Field a -> F.Field T.SqlBool
(.==) = C.binOp (HPQ.:==)

infix 4 ./=
(./=) :: Field a -> Field a -> F.Field T.SqlBool
(./=) = C.binOp (HPQ.:<>)

infix 4 .===
-- | A polymorphic equality operator that works for all types that you
-- have run `makeAdaptorAndInstance` on.  This may be unified with
-- `.==` in a future version.
(.===) :: D.Default O.EqPP fields fields => fields -> fields -> F.Field T.SqlBool
(.===) = (O..==)

infix 4 ./==
-- | A polymorphic inequality operator that works for all types that
-- you have run `makeAdaptorAndInstance` on.  This may be unified with
-- `./=` in a future version.
(./==) :: D.Default O.EqPP fields fields => fields -> fields -> F.Field T.SqlBool
(./==) = Opaleye.Operators.not .: (O..==)

infix 4 .>
(.>) :: Ord.SqlOrd a => Field a -> Field a -> F.Field T.SqlBool
(.>) = unsafeGt

infix 4 .<
(.<) :: Ord.SqlOrd a => Field a -> Field a -> F.Field T.SqlBool
(.<) = C.binOp (HPQ.:<)

infix 4 .<=
(.<=) :: Ord.SqlOrd a => Field a -> Field a -> F.Field T.SqlBool
(.<=) = C.binOp (HPQ.:<=)

infix 4 .>=
(.>=) :: Ord.SqlOrd a => Field a -> Field a -> F.Field T.SqlBool
(.>=) = C.binOp (HPQ.:>=)

-- | Integral division, named after 'Prelude.quot'.  It maps to the
-- @/@ operator in Postgres.
quot_ :: C.SqlIntegral a => Field a -> Field a -> Field a
quot_ = C.binOp (HPQ.:/)

-- | The remainder of integral division, named after 'Prelude.rem'.
-- It maps to 'MOD' ('%') in Postgres, confusingly described as
-- "modulo (remainder)".
rem_ :: C.SqlIntegral a => Field a -> Field a -> Field a
rem_ = C.binOp HPQ.OpMod

-- | Select the first case for which the condition is true.
case_ :: [(F.Field T.SqlBool, Field_ n a)] -> Field_ n a -> Field_ n a
case_ = unsafeCase_

-- | Monomorphic if\/then\/else.
--
-- This may be replaced by 'ifThenElseMany' in a future version.
ifThenElse :: F.Field T.SqlBool -> Field_ n a -> Field_ n a -> Field_ n a
ifThenElse = unsafeIfThenElse

-- | Polymorphic if\/then\/else.
ifThenElseMany :: D.Default O.IfPP fields fields
               => F.Field T.SqlBool
               -> fields
               -> fields
               -> fields
ifThenElseMany = O.ifExplict D.def

infixr 2 .||

-- | Boolean or
(.||) :: F.Field T.SqlBool -> F.Field T.SqlBool -> F.Field T.SqlBool
(.||) = (O..||)

infixr 3 .&&

-- | Boolean and
(.&&) :: F.Field T.SqlBool -> F.Field T.SqlBool -> F.Field T.SqlBool
(.&&) = (O..&&)

-- | Boolean not
not :: F.Field T.SqlBool -> F.Field T.SqlBool
not = O.not

-- | True when any element of the container is true
ors :: F.Foldable f => f (F.Field T.SqlBool) -> F.Field T.SqlBool
ors = F.foldl' (.||) (T.sqlBool False)

-- | Concatenate 'F.Field' 'T.SqlText'
(.++) :: F.Field T.SqlText -> F.Field T.SqlText -> F.Field T.SqlText
(.++) = C.binOp (HPQ.:||)

-- | To lowercase
lower :: F.Field T.SqlText -> F.Field T.SqlText
lower = C.unOp HPQ.OpLower

-- | To uppercase
upper :: F.Field T.SqlText -> F.Field T.SqlText
upper = C.unOp HPQ.OpUpper

-- | Postgres @LIKE@ operator
like :: F.Field T.SqlText -> F.Field T.SqlText -> F.Field T.SqlBool
like = C.binOp HPQ.OpLike

-- | Postgres @ILIKE@ operator
ilike :: F.Field T.SqlText -> F.Field T.SqlText -> F.Field T.SqlBool
ilike = C.binOp HPQ.OpILike

sqlLength :: C.SqlString a => F.Field a -> F.Field T.SqlInt4
sqlLength  (Column e) = Column (HPQ.FunExpr "length" [e])

-- | 'in_' is designed to be used in prefix form.
--
-- 'in_' @validUsers@ @user@ checks whether @user@ is a valid user.
-- 'in_' @validUsers@ is a function which checks whether a user is a
-- valid user.
in_ :: (Functor f, F.Foldable f) => f (Field a) -> Field a -> F.Field T.SqlBool
in_ fcas (Column a) = case NEL.nonEmpty (F.toList fcas) of
   Nothing -> T.sqlBool False
   Just xs -> Column $ HPQ.BinExpr HPQ.OpIn a (HPQ.ListExpr (fmap C.unColumn xs))

-- | True if the first argument occurs amongst the rows of the second,
-- false otherwise.
--
-- This operation is equivalent to Postgres's @IN@ operator.
inSelect :: D.Default O.EqPP fields fields
         => fields -> S.Select fields -> S.Select (F.Field T.SqlBool)
inSelect c q = E.exists $ proc () -> do
  r <- q -< ()
  restrict -< c .=== r
  A.returnA -< r

-- | Class of Postgres types that represent json values.
-- Used to overload functions and operators that work on both 'T.SqlJson' and 'T.SqlJsonb'.
--
-- Warning: making additional instances of this class can lead to broken code!
class SqlIsJson json

instance SqlIsJson T.SqlJson
instance SqlIsJson T.SqlJsonb

-- | Class of Postgres types that can be used to index json values.
--
-- Warning: making additional instances of this class can lead to broken code!
class SqlJsonIndex a

-- | Use 'SqlJsonIndex' instead. Will be deprecated in a future version.
type PGJsonIndex = SqlJsonIndex

instance SqlJsonIndex T.SqlInt4
instance SqlJsonIndex T.SqlInt8
instance SqlJsonIndex T.SqlText

-- | Get JSON object field by key.
infixl 8 .->
(.->) :: (SqlIsJson json, SqlJsonIndex k)
      => F.FieldNullable json -- ^
      -> F.Field k -- ^ key or index
      -> F.FieldNullable json
(.->) = C.binOp (HPQ.:->)

-- | Get JSON object field as text.
infixl 8 .->>
(.->>) :: (SqlIsJson json, SqlJsonIndex k)
       => F.FieldNullable json -- ^
       -> F.Field k -- ^ key or index
       -> F.FieldNullable T.SqlText
(.->>) = C.binOp (HPQ.:->>)

-- | Get JSON object at specified path.
infixl 8 .#>
(.#>) :: (SqlIsJson json)
      => F.FieldNullable json -- ^
      -> Field (T.SqlArray T.SqlText) -- ^ path
      -> F.FieldNullable json
(.#>) = C.binOp (HPQ.:#>)

-- | Get JSON object at specified path as text.
infixl 8 .#>>
(.#>>) :: (SqlIsJson json)
       => F.FieldNullable json -- ^
       -> Field (T.SqlArray T.SqlText) -- ^ path
       -> F.FieldNullable T.SqlText
(.#>>) = C.binOp (HPQ.:#>>)

-- | Does the left JSON value contain within it the right value?
infix 4 .@>
(.@>) :: F.Field T.SqlJsonb -> F.Field T.SqlJsonb -> F.Field T.SqlBool
(.@>) = C.binOp (HPQ.:@>)

-- | Is the left JSON value contained within the right value?
infix 4 .<@
(.<@) :: F.Field T.SqlJsonb -> F.Field T.SqlJsonb -> F.Field T.SqlBool
(.<@) = C.binOp (HPQ.:<@)

-- | Does the key/element string exist within the JSON value?
infix 4 .?
(.?) :: F.Field T.SqlJsonb -> F.Field T.SqlText -> F.Field T.SqlBool
(.?) = C.binOp (HPQ.:?)

-- | Do any of these key/element strings exist?
infix 4 .?|
(.?|) :: F.Field T.SqlJsonb
      -> Field (T.SqlArray T.SqlText)
      -> F.Field T.SqlBool
(.?|) = C.binOp (HPQ.:?|)

-- | Do all of these key/element strings exist?
infix 4 .?&
(.?&) :: F.Field T.SqlJsonb
      -> Field (T.SqlArray T.SqlText)
      -> F.Field T.SqlBool
(.?&) = C.binOp (HPQ.:?&)

emptyArray :: T.IsSqlType a => Field (T.SqlArray_ n a)
emptyArray = T.sqlArray id []

-- | Append two 'T.SqlArray's
arrayAppend :: F.Field (T.SqlArray_ n a) -> F.Field (T.SqlArray_ n a) -> F.Field (T.SqlArray_ n a)
arrayAppend = C.binOp (HPQ.:||)

-- | Prepend an element to a 'T.SqlArray'
arrayPrepend :: Field_ n a -> Field (T.SqlArray_ n a) -> Field (T.SqlArray_ n a)
arrayPrepend (Column e) (Column es) = Column (HPQ.FunExpr "array_prepend" [e, es])

-- | Remove all instances of an element from a 'T.SqlArray'
arrayRemove :: Field_ n a -> Field (T.SqlArray_ n a) -> Field (T.SqlArray_ n a)
arrayRemove (Column e) (Column es) = Column (HPQ.FunExpr "array_remove" [es, e])

-- | Remove all 'NULL' values from a 'T.SqlArray'
arrayRemoveNulls :: Field (T.SqlArray_ Nullable a) -> Field (T.SqlArray a)
arrayRemoveNulls = Column.unsafeCoerceColumn . arrayRemove F.null

singletonArray :: T.IsSqlType a => Field_ n a -> Field (T.SqlArray_ n a)
singletonArray x = arrayPrepend x emptyArray

index :: (C.SqlIntegral n) => Field (T.SqlArray_ n' a) -> Field n -> FieldNullable a
index (Column a) (Column b) = Column (HPQ.ArrayIndex a b)

-- | Postgres's @array_position@
arrayPosition :: F.Field (T.SqlArray_ n a) -- ^ Haystack
              -> F.Field_ n a -- ^ Needle
              -> F.FieldNullable T.SqlInt4
arrayPosition (Column fs) (Column f') =
  C.Column (HPQ.FunExpr "array_position" [fs , f'])

-- | Whether the element (needle) exists in the array (haystack).
-- N.B. this is implemented hackily using @array_position@.  If you
-- need it to be implemented using @= any@ then please open an issue.
sqlElem :: F.Field_ n a -- ^ Needle
        -> F.Field (T.SqlArray_ n a) -- ^ Haystack
        -> F.Field T.SqlBool
sqlElem f fs = (O.not . F.isNull . arrayPosition fs) f

overlap :: Field (T.SqlRange a) -> Field (T.SqlRange a) -> F.Field T.SqlBool
overlap = C.binOp (HPQ.:&&)

liesWithin :: T.IsRangeType a => Field a -> Field (T.SqlRange a) -> F.Field T.SqlBool
liesWithin = C.binOp (HPQ.:<@)

-- | Access the upper bound of a range. For discrete range types it is the exclusive bound.
upperBound :: T.IsRangeType a => Field (T.SqlRange a) -> FieldNullable a
upperBound (Column range) = Column $ HPQ.FunExpr "upper" [range]

-- | Access the lower bound of a range. For discrete range types it is the inclusive bound.
lowerBound :: T.IsRangeType a => Field (T.SqlRange a) -> FieldNullable a
lowerBound (Column range) = Column $ HPQ.FunExpr "lower" [range]

infix 4 .<<
(.<<) :: Field (T.SqlRange a) -> Field (T.SqlRange a) -> F.Field T.SqlBool
(.<<) = C.binOp (HPQ.:<<)

infix 4 .>>
(.>>) :: Field (T.SqlRange a) -> Field (T.SqlRange a) -> F.Field T.SqlBool
(.>>) = C.binOp (HPQ.:>>)

infix 4 .&<
(.&<) :: Field (T.SqlRange a) -> Field (T.SqlRange a) -> F.Field T.SqlBool
(.&<) = C.binOp (HPQ.:&<)

infix 4 .&>
(.&>) :: Field (T.SqlRange a) -> Field (T.SqlRange a) -> F.Field T.SqlBool
(.&>) = C.binOp (HPQ.:&>)

infix 4 .-|-
(.-|-) :: Field (T.SqlRange a) -> Field (T.SqlRange a) -> F.Field T.SqlBool
(.-|-) = C.binOp (HPQ.:-|-)

timestamptzAtTimeZone :: F.Field T.SqlTimestamptz
                      -> F.Field T.SqlText
                      -> F.Field T.SqlTimestamp
timestamptzAtTimeZone = C.binOp HPQ.OpAtTimeZone

dateOfTimestamp :: F.Field T.SqlTimestamp -> F.Field T.SqlDate
dateOfTimestamp (Column e) = Column (HPQ.FunExpr "date" [e])

-- | @IntervalNum from to@ determines from which date or time types an interval
-- can be added ('addInterval') or subtracted ('minusInterval`) and which is the
-- resulting type.
--
-- The instances should correspond to the interval + and - operations listed in:
--
-- https://www.postgresql.org/docs/current/functions-datetime.html#OPERATORS-DATETIME-TABLE
class IntervalNum from to | from -> to

instance IntervalNum T.SqlDate        T.SqlTimestamp
instance IntervalNum T.SqlInterval    T.SqlInterval
instance IntervalNum T.SqlTimestamp   T.SqlTimestamp
instance IntervalNum T.SqlTimestamptz T.SqlTimestamptz
instance IntervalNum T.SqlTime        T.SqlTime

addInterval :: IntervalNum from to => F.Field from -> F.Field T.SqlInterval -> F.Field to
addInterval = C.binOp (HPQ.:+)

minusInterval :: IntervalNum from to => F.Field from -> F.Field T.SqlInterval -> F.Field to
minusInterval = C.binOp (HPQ.:-)

-- | Current date and time (start of current transaction)
now :: F.Field T.SqlTimestamptz
now = Column $ HPQ.FunExpr "now" []

data TimestampPrecision =
  MicrosecondsPrecision
  | MillisecondsPrecision
  | SecondPrecision
  | MinutePrecision
  | HourPrecision
  | DayPrecision
  | WeekPrecision
  | MonthPrecision
  | QuarterPrecision
  | YearPrecision
  | DecadePrecision
  | CenturyPrecision
  | MillenniumPrecision
  deriving Show

precisionToExpr :: TimestampPrecision -> HPQ.PrimExpr
precisionToExpr p = HPQ.ConstExpr . HPQ.ByteStringLit . TE.encodeUtf8 . Text.toLower . Text.dropEnd 9 . Text.pack $ show p

dateTruncTimestamp :: TimestampPrecision -> F.Field T.SqlTimestamp -> F.Field T.SqlTimestamp
dateTruncTimestamp p (Column e) = Column $ HPQ.FunExpr "date_trunc" [(precisionToExpr p), e]

dateTruncTimestamptz :: TimestampPrecision -> F.Field T.SqlTimestamptz -> F.Field T.SqlTimestamptz
dateTruncTimestamptz p (Column e) = Column $ HPQ.FunExpr "date_trunc" [(precisionToExpr p), e]
