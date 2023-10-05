{-# LANGUAGE TupleSections #-}
module Opaleye.Internal.Aggregate where

import           Control.Applicative (Applicative, liftA2, pure, (<*>))
import           Data.Foldable (toList)
import           Data.Traversable (for)

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

import qualified Opaleye.Field as F
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.Order as O
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.SqlTypes as T

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

{-|
An 'Aggregator' takes a collection of rows of type @a@, groups
them, and transforms each group into a single row of type @b@. This
corresponds to aggregators using @GROUP BY@ in SQL.

You should combine basic 'Aggregator's into 'Aggregator's on compound
types by using the operations in "Data.Profunctor.Product".

An 'Aggregator' corresponds closely to a 'Control.Foldl.Fold' from the
@foldl@ package.  Whereas an 'Aggregator' @a@ @b@ takes each group of
type @a@ to a single row of type @b@, a 'Control.Foldl.Fold' @a@ @b@
takes a list of @a@ and returns a single value of type @b@.
-}
newtype Aggregator a b =
  Aggregator (PM.PackMap HPQ.Aggregate HPQ.PrimExpr a b)

makeAggr' :: Maybe HPQ.AggrOp -> Aggregator (C.Field_ n a) (C.Field_ n' b)
makeAggr' mAggrOp = P.dimap C.unColumn C.Column $ Aggregator (PM.PackMap
  (\f e -> f (aggr e)))
  where
    aggr = case mAggrOp of
      Nothing -> HPQ.GroupBy
      Just op -> \e -> HPQ.Aggregate (HPQ.Aggr op [e] [] HPQ.AggrAll [] Nothing)

makeAggr :: HPQ.AggrOp -> Aggregator (C.Field_ n a) (C.Field_ n' b)
makeAggr = makeAggr' . Just

makeAggrExplicit :: U.Unpackspec a a -> HPQ.AggrOp -> Aggregator a (C.Field_ n b)
makeAggrExplicit unpackspec op =
  C.Column <$> Aggregator (PM.PackMap (\f e -> f (aggr e)))
  where
    aggr a = HPQ.Aggregate (HPQ.Aggr op exprs [] HPQ.AggrAll [] Nothing)
      where
        exprs = U.collectPEs unpackspec a


-- | Order the values within each aggregation in `Aggregator` using
-- the given ordering. This is only relevant for aggregations that
-- depend on the order they get their elements, like
-- `Opaleye.Aggregate.arrayAgg` and `Opaleye.Aggregate.stringAgg`.
--
-- You can either apply it to an aggregation of multiple columns, in
-- which case it will apply to all aggregation functions in there
--
-- Example:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray b))
-- > x = orderAggregate (asc snd) $ p2 (arrayAgg, arrayAgg)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(b ORDER BY b ASC)
-- FROM (SELECT a, b FROM ...)
-- @
--
-- Or you can apply it to a single column, and then compose the aggregations
-- afterwards.
--
-- Example:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray a))
-- > x = (,) <$> orderAggregate (asc snd) (lmap fst arrayAgg)
-- >         <*> orderAggregate (desc snd) (lmap fst arrayAgg)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(a ORDER BY b DESC)
-- FROM (SELECT a, b FROM ...)
-- @

orderAggregate :: O.Order a -> Aggregator a b -> Aggregator a b
orderAggregate o (Aggregator (PM.PackMap pm)) = Aggregator (PM.PackMap
  (\f c -> pm (f . setOrder (O.orderExprs c o)) c))
  where
    setOrder _ (HPQ.GroupBy e) = HPQ.GroupBy e
    setOrder order (HPQ.Aggregate aggr) =
      HPQ.Aggregate aggr
        { HPQ.aggrOrder = order
        }

runAggregator
  :: Applicative f
  => Aggregator a b
  -> (HPQ.Aggregate -> f HPQ.PrimExpr)
  -> a -> f b
runAggregator (Aggregator a) = PM.traversePM a

-- For rel8.
--
-- Like https://www.stackage.org/haddock/lts-19.10/base-4.15.1.0/Control-Arrow.html#t:ArrowApply
aggregatorApply :: Aggregator (Aggregator a b, a) b
aggregatorApply = Aggregator $ PM.PackMap $ \f (agg, a) ->
  case agg of
    Aggregator (PM.PackMap inner) -> inner f a

-- In Postgres (and, I believe, standard SQL) "aggregate functions are
-- not allowed in FROM clause of their own query level".  There
-- doesn't seem to be any fundamental reason for this, but we are
-- stuck with it.  That means that in a lateral subquery containing an
-- aggregation over a field C from a previous subquery we have to
-- create a new field name for C before we are allowed to aggregate it!
-- For more information see
--
--     https://www.postgresql.org/message-id/20200513110251.GC24083%40cloudinit-builder
--
--     https://github.com/tomjaguarpaw/haskell-opaleye/pull/460#issuecomment-626716160
--
-- Instead of detecting when we are aggregating over a field from a
-- previous query we just create new names for all field before we
-- aggregate.  On the other hand, referring to a field from a previous
-- query in an ORDER BY expression is totally fine!
aggregateU :: Aggregator a b
           -> (a, PQ.PrimQuery, T.Tag) -> (b, PQ.PrimQuery)
aggregateU agg (c0, primQ, t0) = (c1, primQ')
  where (c1, projPEs_inners) =
          PM.run (runAggregator agg (extractAggregateFields t0) c0)

        projPEs = map fst projPEs_inners
        inners  = concatMap snd projPEs_inners

        primQ' = PQ.Aggregate projPEs (PQ.Rebind True inners primQ)

extractAggregateFields
  :: Traversable t
  => T.Tag
  -> (t HPQ.PrimExpr)
  -> PM.PM [((HPQ.Symbol,
              t HPQ.Symbol),
              PQ.Bindings HPQ.PrimExpr)]
           HPQ.PrimExpr
extractAggregateFields tag agg = do
  i <- PM.new

  let souter = HPQ.Symbol ("result" ++ i) tag

  bindings <- for agg $ \pe -> do
    j <- PM.new
    let sinner = HPQ.Symbol ("inner" ++ j) tag
    pure (sinner, pe)

  let agg' = fmap fst bindings

  PM.write ((souter, agg'), toList bindings)

  pure (HPQ.AttrExpr souter)

unsafeMax :: Aggregator (C.Field a) (C.Field a)
unsafeMax = makeAggr HPQ.AggrMax

unsafeMin :: Aggregator (C.Field a) (C.Field a)
unsafeMin = makeAggr HPQ.AggrMin

unsafeAvg :: Aggregator (C.Field a) (C.Field a)
unsafeAvg = makeAggr HPQ.AggrAvg

unsafeSum :: Aggregator (C.Field a) (C.Field a)
unsafeSum = makeAggr HPQ.AggrSum

-- | Aggregate only rows matching the given predicate
filterWhereInternal
  :: (F.FieldNullable T.SqlBool -> b -> mb)
  -> (a -> F.Field T.SqlBool)
  -> Aggregator a b
  -> Aggregator a mb
filterWhereInternal maybeField predicate aggregator =
  case liftA2 maybeField true aggregator of
    Aggregator (PM.PackMap pm) ->
      Aggregator (PM.PackMap (\f c -> pm (f . setFilter c) c))
  where
    true = P.lmap (const (T.sqlBool True)) (makeAggr HPQ.AggrBoolAnd)
    setFilter _ (HPQ.GroupBy e) = HPQ.GroupBy e
    setFilter row (HPQ.Aggregate aggr) =
      HPQ.Aggregate aggr
        { HPQ.aggrFilter = aggrFilter'
        }
      where
        C.Column cond' = predicate row
        aggrFilter' = Just $ case HPQ.aggrFilter aggr of
          Nothing -> cond'
          Just cond -> HPQ.BinExpr HPQ.OpAnd cond cond'

withinGroup :: O.Order a -> Aggregator a b -> Aggregator a b
withinGroup o (Aggregator (PM.PackMap pm)) = Aggregator (PM.PackMap
  (\f c -> pm (f . setOrder (O.orderExprs c o)) c))
  where
    setOrder _ (HPQ.GroupBy e) = HPQ.GroupBy e
    setOrder order (HPQ.Aggregate aggr) =
      HPQ.Aggregate aggr
        { HPQ.aggrGroup = order
        }

-- { Boilerplate instances

instance Functor (Aggregator a) where
  fmap f (Aggregator g) = Aggregator (fmap f g)

instance Applicative (Aggregator a) where
  pure = Aggregator . pure
  Aggregator f <*> Aggregator x = Aggregator (f <*> x)

instance P.Profunctor Aggregator where
  dimap f g (Aggregator q) = Aggregator (P.dimap f g q)

instance PP.ProductProfunctor Aggregator where
  purePP = pure
  (****) = (<*>)

instance PP.SumProfunctor Aggregator where
  Aggregator x1 +++! Aggregator x2 = Aggregator (x1 PP.+++! x2)

-- }
