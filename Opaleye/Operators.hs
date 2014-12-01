module Opaleye.Operators (module Opaleye.Operators,
                          (.==),
                          (.>),
                          case_,
                          ifThenElse) where

import           Opaleye.Internal.Column (Column(Column), (.==), case_, (.>),
                                          ifThenElse)
import qualified Opaleye.Internal.Column as C
import           Opaleye.Internal.QueryArr (QueryArr(QueryArr))
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

{-| Restrict query results to a particular condition.  Corresponds to
    the guard method of the MonadPlus class.
-}
restrict :: QueryArr (Column Bool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.restrict predicate primQ, t0)

doubleOfInt :: Column Int -> Column Double
doubleOfInt (Column e) = Column (HPQ.CastExpr "double precision" e)

infix 4 .<
(.<) :: Column a -> Column a -> Column Bool
(.<) = C.binOp HPQ.OpLt

infix 4 .<=
(.<=) :: Column a -> Column a -> Column Bool
(.<=) = C.binOp HPQ.OpLtEq

infix 4 .>=
(.>=) :: Column a -> Column a -> Column Bool
(.>=) = C.binOp HPQ.OpGtEq

infixr 3 .&&
(.&&) :: Column Bool -> Column Bool -> Column Bool
(.&&) = C.binOp HPQ.OpAnd

infixr 2 .||
(.||) :: Column Bool -> Column Bool -> Column Bool
(.||) = C.binOp HPQ.OpOr

not :: Column Bool -> Column Bool
not = C.unOp HPQ.OpNot

-- FIXME: Should we get rid of this and just use a monoid instance?
(.++) :: Column String -> Column String -> Column String
(.++) = C.binOp (HPQ.OpOther "||")

(./=) :: Column a -> Column a -> Column Bool
(./=) = C.binOp HPQ.OpNotEq

lower :: Column a -> Column a
lower = C.unOp (HPQ.UnOpOther "lower")

upper :: Column a -> Column a
upper = C.unOp (HPQ.UnOpOther "upper")
