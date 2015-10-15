module Opaleye.Internal.Optimize where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Data.List.NonEmpty as NEL

optimize :: PQ.PrimQuery -> PQ.PrimQuery
optimize = mergeProduct . removeUnit

removeUnit :: PQ.PrimQuery -> PQ.PrimQuery
removeUnit = PQ.foldPrimQuery (PQ.Unit, PQ.BaseTable, product, PQ.Aggregate,
                                PQ.Order, PQ.Limit, PQ.Join, PQ.Values,
                                PQ.Binary, PQ.Label)
  where product pqs pes = PQ.Product pqs' pes
          where pqs' = case NEL.filter (not . PQ.isUnit) pqs of
                         [] -> return PQ.Unit
                         xs -> NEL.fromList xs

mergeProduct :: PQ.PrimQuery -> PQ.PrimQuery
mergeProduct = PQ.foldPrimQuery (PQ.Unit, PQ.BaseTable, product, PQ.Aggregate,
                                PQ.Order, PQ.Limit, PQ.Join, PQ.Values,
                                PQ.Binary, PQ.Label)
  where product pqs pes = PQ.Product pqs' (pes ++ pes')
          where pqs' = pqs >>= queries
                queries (PQ.Product qs _) = qs
                queries q = return q
                pes' = NEL.toList pqs >>= conds
                conds (PQ.Product _ cs) = cs
                conds _ = []
