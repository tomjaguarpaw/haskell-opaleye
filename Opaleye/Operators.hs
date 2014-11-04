module Opaleye.Operators (module Opaleye.Operators, (.==)) where

import           Opaleye.Column (Column(Column), (.==))
import           Opaleye.QueryArr (QueryArr(QueryArr))
import qualified Database.HaskellDB.PrimQuery as PQ

restrict :: QueryArr (Column Bool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.Restrict predicate primQ, t0)

doubleOfInt :: Column Int -> Column Double
doubleOfInt (Column e) = (Column (PQ.CastExpr "double precision" e))
