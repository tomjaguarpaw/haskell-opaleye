{-# LANGUAGE LambdaCase #-}

module Opaleye.Internal.Optimize where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.Internal.Helpers   ((.:))

import qualified Data.List.NonEmpty as NEL
import           Data.Semigroup ((<>))

import           Control.Applicative ((<$>), (<*>), liftA2, pure)
import           Control.Arrow (first)

optimize :: PQ.PrimQuery' a -> PQ.PrimQuery' a
optimize = PQ.foldPrimQuery (noSingletonProduct
                             `PQ.composePrimQueryFold` mergeProduct
                             `PQ.composePrimQueryFold` removeUnit)

removeUnit :: PQ.PrimQueryFoldP a (PQ.PrimQuery' a) (PQ.PrimQuery' a)
removeUnit = PQ.primQueryFoldDefault { PQ.product   = product }
  where product pqs = PQ.Product pqs'
          where pqs' = case NEL.nonEmpty (NEL.filter (not . PQ.isUnit . snd) pqs) of
                         Nothing -> return (pure PQ.Unit)
                         Just xs -> xs

mergeProduct :: PQ.PrimQueryFoldP a (PQ.PrimQuery' a) (PQ.PrimQuery' a)
mergeProduct = PQ.primQueryFoldDefault { PQ.product   = product }
  where product pqs pes = PQ.Product pqs' (pes ++ pes')
          where pqs' = pqs >>= queries
                queries (lat, PQ.Product qs _) = fmap (first (lat <>)) qs
                queries q = return q
                pes' = NEL.toList pqs >>= conds
                conds (_lat, PQ.Product _ cs) = cs
                conds _ = []

noSingletonProduct :: PQ.PrimQueryFoldP a (PQ.PrimQuery' a) (PQ.PrimQuery' a)
noSingletonProduct = PQ.primQueryFoldDefault { PQ.product = product }
  where product pqs conds = case (NEL.uncons pqs, conds) of
          (((PQ.NonLateral, x), Nothing), []) -> x
          _ -> PQ.Product pqs conds

removeEmpty :: PQ.PrimQuery' a -> Maybe (PQ.PrimQuery' b)
removeEmpty = PQ.foldPrimQuery PQ.PrimQueryFold {
    PQ.unit      = return PQ.Unit
  , PQ.empty     = const Nothing
  , PQ.baseTable = return .: PQ.BaseTable
  , PQ.product   = let sequenceOf l = traverseOf l id
                       traverseOf = id
                       _2 = traverse
                   in
                   \x y -> PQ.Product <$> sequenceOf (traverse._2) x
                                      <*> pure y
  , PQ.aggregate = fmap . PQ.Aggregate
  , PQ.distinctOnOrderBy = \mDistinctOns -> fmap . PQ.DistinctOnOrderBy mDistinctOns
  , PQ.limit     = fmap . PQ.Limit
  , PQ.join      = \jt pe pq1 pq2 -> PQ.Join jt pe <$> sequence pq1 <*> sequence pq2
  , PQ.semijoin  = liftA2 . PQ.Semijoin
  , PQ.exists    = fmap . PQ.Exists
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
  , PQ.rebind    = \b -> fmap . PQ.Rebind b
  , PQ.forUpdate = fmap PQ.ForUpdate
  }
  where -- If only the first argument is Just, do n1 on it
        -- If only the second argument is Just, do n2 on it
        binary n1 n2 jj = \case
          (Nothing, Nothing)   -> Nothing
          (Nothing, Just pq2)  -> n2 pq2
          (Just pq1, Nothing)  -> n1 pq1
          (Just pq1, Just pq2) -> Just (PQ.Binary jj (pq1, pq2))
