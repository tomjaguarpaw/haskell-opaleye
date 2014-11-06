module Opaleye.Internal.Optimize where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.NEList as NE

optimize :: PQ.PrimQuery -> PQ.PrimQuery
optimize = removeUnit

removeUnit :: PQ.PrimQuery -> PQ.PrimQuery
removeUnit = PQ.foldPrimQuery (PQ.Unit, PQ.BaseTable, product, PQ.Aggregate,
                                PQ.Order, PQ.Limit, PQ.LeftJoin)
  where product pqs pes = flip PQ.Product pes $ case notUnit of
          []   -> NE.NEList PQ.Unit []
          x:xs -> NE.NEList x       xs
          where notUnit = (filter (not . PQ.isUnit) . NE.toList) pqs
