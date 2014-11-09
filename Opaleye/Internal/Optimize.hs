module Opaleye.Internal.Optimize where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.NEList as NE

optimize :: PQ.PrimQuery -> PQ.PrimQuery
optimize = mergeProduct . removeUnit

removeUnit :: PQ.PrimQuery -> PQ.PrimQuery
removeUnit = PQ.foldPrimQuery (PQ.Unit, PQ.BaseTable, product, PQ.Aggregate,
                                PQ.Order, PQ.Limit, PQ.Join, PQ.Values)
  where product pqs pes = flip PQ.Product pes $ case notUnit of
          []   -> NE.NEList PQ.Unit []
          x:xs -> NE.NEList x       xs
          where notUnit = (filter (not . PQ.isUnit) . NE.toList) pqs

mergeProduct :: PQ.PrimQuery -> PQ.PrimQuery
mergeProduct = PQ.foldPrimQuery (PQ.Unit, PQ.BaseTable, product, PQ.Aggregate,
                                PQ.Order, PQ.Limit, PQ.Join, PQ.Values)
  where product pqs pes = PQ.Product pqs' (pes ++ pes')
          where pqs' = pqs >>= queries
                queries (PQ.Product qs _) = qs
                queries q = NE.NEList q []
                pes' = NE.toList pqs >>= conds
                conds (PQ.Product _ cs) = cs
                conds _ = []
