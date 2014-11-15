module Opaleye.Operators (module Opaleye.Operators, (.==), case_) where

import           Opaleye.Internal.Column (Column(Column), (.==), case_)
import           Opaleye.QueryArr (QueryArr(QueryArr))
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Database.HaskellDB.PrimQuery as HPQ

restrict :: QueryArr (Column Bool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.restrict predicate primQ, t0)

doubleOfInt :: Column Int -> Column Double
doubleOfInt (Column e) = Column (HPQ.CastExpr "double precision" e)
