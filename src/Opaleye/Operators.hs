module Opaleye.Operators (module Opaleye.Operators) where

import           Opaleye.Internal.Column (Column(Column), unsafeCase_,
                                          unsafeIfThenElse, unsafeGt, unsafeEq)
import qualified Opaleye.Internal.Column as C
import           Opaleye.Internal.QueryArr (QueryArr(QueryArr))
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.PGTypes as T

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

{-| Restrict query results to a particular condition.  Corresponds to
    the guard method of the MonadPlus class.
-}
restrict :: QueryArr (Column T.PGBool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.restrict predicate primQ, t0)

doubleOfInt :: Column T.PGInt4 -> Column T.PGFloat8
doubleOfInt (Column e) = Column (HPQ.CastExpr "float8" e)

infix 4 .==
(.==) :: Column a -> Column a -> Column T.PGBool
(.==) = unsafeEq

infix 4 ./=
(./=) :: (T.ComparableColumn a) => Column a -> Column a -> Column T.PGBool
(./=) = C.binOp HPQ.OpNotEq

infix 4 .>
(.>) :: (T.ComparableColumn a) => Column a -> Column a -> Column T.PGBool
(.>) = unsafeGt

infix 4 .<
(.<) :: (T.ComparableColumn a) => Column a -> Column a -> Column T.PGBool
(.<) = C.binOp HPQ.OpLt

infix 4 .<=
(.<=) :: (T.ComparableColumn a) => Column a -> Column a -> Column T.PGBool
(.<=) = C.binOp HPQ.OpLtEq

infix 4 .>=
(.>=) :: (T.ComparableColumn a) => Column a -> Column a -> Column T.PGBool
(.>=) = C.binOp HPQ.OpGtEq

case_ :: [(Column T.PGBool, Column a)] -> Column a -> Column a
case_ = unsafeCase_

ifThenElse :: Column T.PGBool -> Column a -> Column a -> Column a
ifThenElse = unsafeIfThenElse

infixr 3 .&&
(.&&) :: Column T.PGBool -> Column T.PGBool -> Column T.PGBool
(.&&) = C.binOp HPQ.OpAnd

infixr 2 .||
(.||) :: Column T.PGBool -> Column T.PGBool -> Column T.PGBool
(.||) = C.binOp HPQ.OpOr

not :: Column T.PGBool -> Column T.PGBool
not = C.unOp HPQ.OpNot

(.++) :: Column T.PGText -> Column T.PGText -> Column T.PGText
(.++) = C.binOp HPQ.OpCat

lower :: Column T.PGText -> Column T.PGText
lower = C.unOp HPQ.OpLower

upper :: Column T.PGText -> Column T.PGText
upper = C.unOp HPQ.OpUpper

like :: Column T.PGText -> Column T.PGText -> Column T.PGBool
like = C.binOp HPQ.OpLike
