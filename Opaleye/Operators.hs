module Opaleye.Operators where

import           Opaleye.Column (Column(Column))
import           Opaleye.QueryArr (QueryArr(QueryArr))
import qualified Database.HaskellDB.PrimQuery as PQ

(.==) :: Column a -> Column a -> Column Bool
Column e .== Column e' = Column (PQ.BinExpr PQ.OpEq e e')

restrict :: QueryArr (Column Bool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.Restrict predicate primQ, t0)
