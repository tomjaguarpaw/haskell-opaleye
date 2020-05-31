{-# LANGUAGE TupleSections #-}
module Opaleye.Internal.Aggregate where

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.Order as O

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
  Aggregator (PM.PackMap (Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct),
                          HPQ.PrimExpr)
                         HPQ.PrimExpr
                         a b)

makeAggr' :: Maybe HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr' mAggrOp = Aggregator (PM.PackMap
  (\f (C.Column e) -> fmap C.Column (f (fmap (, [], HPQ.AggrAll) mAggrOp, e))))

makeAggr :: HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr = makeAggr' . Just

-- | Order the values within each aggregation in `Aggregator` using
-- the given ordering. This is only relevant for aggregations that
-- depend on the order they get their elements, like
-- `Opaleye.Aggregate.arrayAgg` and `Opaleye.Aggregate.stringAgg`.
--
-- You can either apply it to an aggregation of multiple columns, in
-- which case it will apply to all aggregation functions in there, or you
-- can apply it to a single column, and then compose the aggregations
-- afterwards. Examples:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray a))
-- > x = (,) <$> orderAggregate (asc snd) (lmap fst arrayAggGrouped)
-- >         <*> orderAggregate (desc snd) (lmap fst arrayAggGrouped)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(a ORDER BY b DESC)
-- FROM (SELECT a, b FROM ...)
-- @
--
-- Or:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray b))
-- > x = orderAggregate (asc snd) $ p2 (arrayAggGrouped, arrayAggGrouped)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(b ORDER BY b ASC)
-- FROM (SELECT a, b FROM ...)
-- @

orderAggregate :: O.Order a -> Aggregator a b -> Aggregator a b
orderAggregate o (Aggregator (PM.PackMap pm)) = Aggregator (PM.PackMap
  (\f c -> pm (f . P.first' (fmap ((\f' (a,b,c') -> (a,f' b,c')) (const $ O.orderExprs c o)))) c))

runAggregator
  :: Applicative f
  => Aggregator a b
  -> ((Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct), HPQ.PrimExpr)
     -> f HPQ.PrimExpr)
  -> a -> f b
runAggregator (Aggregator a) = PM.traversePM a

-- In Postgres (and, I believe, standard SQL) "aggregate functions are
-- not allowed in FROM clause of their own query level".  There
-- doesn't seem to be any fundamental reason for this, but we are
-- stuck with it.  That means that in a lateral subquery containing an
-- aggregation over a column C from a previous subquery we have to
-- create a new column name for C before we are allowed to aggregate it!
-- For more information see
--
--     https://www.postgresql.org/message-id/20200513110251.GC24083%40cloudinit-builder
--
--     https://github.com/tomjaguarpaw/haskell-opaleye/pull/460#issuecomment-626716160
-- Instead of detecting when we are aggregating over a column from a
-- previous query we just create new names for all columns before we
-- aggregate.
aggregateU :: Aggregator a b
           -> (a, PQ.PrimQuery, T.Tag) -> (b, PQ.PrimQuery, T.Tag)
aggregateU agg (c0, primQ, t0) = (c1, primQ', T.next t0)
  where (c1, projPEs_inners) =
          PM.run (runAggregator agg (extractAggregateFields t0) c0)

        projPEs = map fst projPEs_inners
        inners  = map snd projPEs_inners

        primQ' = PQ.PQAggregate PQ.Aggregate {
            PQ.aggregateOperations  = projPEs
          , PQ.aggregateProjections = inners
          , PQ.aggregateSubquery    = primQ
          }

extractAggregateFields
  :: T.Tag
  -> (m, HPQ.PrimExpr)
  -> PM.PM [((HPQ.Symbol,
              (m, HPQ.Symbol)),
              (HPQ.Symbol, HPQ.PrimExpr))]
           HPQ.PrimExpr
extractAggregateFields tag (m, pe) = do
  i <- PM.new

  let souter = HPQ.Symbol ("result" ++ i) tag
      sinner = HPQ.Symbol ("inner" ++ i) tag

  PM.write ((souter, (m, sinner)), (sinner, pe))

  pure (HPQ.AttrExpr souter)

-- { Boilerplate instances

instance Functor (Aggregator a) where
  fmap f (Aggregator g) = Aggregator (fmap f g)

instance Applicative (Aggregator a) where
  pure = Aggregator . pure
  Aggregator f <*> Aggregator x = Aggregator (f <*> x)

instance P.Profunctor Aggregator where
  dimap f g (Aggregator q) = Aggregator (P.dimap f g q)

instance PP.ProductProfunctor Aggregator where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor Aggregator where
  Aggregator x1 +++! Aggregator x2 = Aggregator (x1 PP.+++! x2)

-- }
