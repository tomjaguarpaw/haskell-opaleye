{-# LANGUAGE LambdaCase #-}

module Opaleye.Internal.Optimize where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.Internal.Helpers   ((.:))

import qualified Data.List.NonEmpty as NEL

import           Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Traversable    as T

optimize :: PQ.PrimQuery' a -> PQ.PrimQuery' a
optimize = mergeProduct . removeUnit

removeUnit :: PQ.PrimQuery' a -> PQ.PrimQuery' a
removeUnit = PQ.foldPrimQuery PQ.primQueryFoldDefault { PQ.product   = product }
  where product pqs pes = PQ.Product pqs' pes
          where pqs' = case NEL.nonEmpty (NEL.filter (not . PQ.isUnit) pqs) of
                         Nothing -> return PQ.Unit
                         Just xs -> xs

mergeProduct :: PQ.PrimQuery' a -> PQ.PrimQuery' a
mergeProduct = PQ.foldPrimQuery PQ.primQueryFoldDefault { PQ.product   = product }
  where product pqs pes = PQ.Product pqs' (pes ++ pes')
          where pqs' = pqs >>= queries
                queries (PQ.Product qs _) = qs
                queries q = return q
                pes' = NEL.toList pqs >>= conds
                conds (PQ.Product _ cs) = cs
                conds _ = []

removeEmpty :: PQ.PrimQuery' a -> Maybe (PQ.PrimQuery' b)
removeEmpty = PQ.foldPrimQuery PQ.PrimQueryFold {
    PQ.unit      = return PQ.Unit
  , PQ.empty     = const Nothing
  , PQ.baseTable = return .: PQ.BaseTable
  , PQ.product   = \x y -> PQ.Product <$> T.sequence x
                                      <*> pure y
  , PQ.aggregate = fmap . PQ.Aggregate
  , PQ.distinctOnOrderBy = \mDistinctOns -> fmap . PQ.DistinctOnOrderBy mDistinctOns
  , PQ.limit     = fmap . PQ.Limit
  , PQ.join      = \jt pe pes1 pes2 pq1 pq2 -> PQ.Join jt pe pes1 pes2 <$> pq1 <*> pq2
  , PQ.existsf   = \b pq1 pq2 -> PQ.Exists b <$> pq1 <*> pq2
  , PQ.values    = return .: PQ.Values
  , PQ.binary    = \case
      -- Some unfortunate duplication here
      PQ.Except       -> binary Just            (const Nothing) PQ.Except
      PQ.Union        -> binary Just            Just            PQ.Union
      PQ.Intersect    -> binary (const Nothing) (const Nothing) PQ.Intersect

      PQ.ExceptAll    -> binary Just            (const Nothing) PQ.ExceptAll
      PQ.UnionAll     -> binary Just            Just            PQ.UnionAll
      PQ.IntersectAll -> binary (const Nothing) (const Nothing) PQ.IntersectAll
  , PQ.label     = fmap . PQ.Label
  , PQ.relExpr   = return .: PQ.RelExpr
  , PQ.rebind    = fmap . PQ.Rebind
  }
  where -- If only the first argument is Just, do n1 on it
        -- If only the second argument is Just, do n2 on it
        binary n1 n2 jj = \case
          (Nothing, Nothing)   -> Nothing
          (Nothing, Just pq2)  -> n2 pq2
          (Just pq1, Nothing)  -> n1 pq1
          (Just pq1, Just pq2) -> Just (PQ.Binary jj (pq1, pq2))
