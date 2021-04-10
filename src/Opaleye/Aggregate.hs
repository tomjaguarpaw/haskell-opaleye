{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Perform aggregation on 'S.Select's.  To aggregate a 'S.Select' you
-- should construct an 'Aggregator' encoding how you want the
-- aggregation to proceed, then call 'aggregate' on it.  The
-- 'Aggregator' should be constructed from the basic 'Aggregator's
-- below by using the combining operations from
-- "Data.Profunctor.Product".

module Opaleye.Aggregate
       (
       -- * Aggregation
         aggregate
       , aggregateLaterally
       , aggregateEasy
       , agg
       , aggregateOrdered
       , distinctAggregator
       , Aggregator
       -- * Basic 'Aggregator's
       , groupBy
       , Opaleye.Aggregate.sum
       , sumInt4
       , sumInt8
       , count
       , countStar
       , avg
       , Opaleye.Aggregate.max
       , Opaleye.Aggregate.min
       , boolOr
       , boolAnd
       , arrayAgg
       , jsonAgg
       , stringAgg
       -- * Counting rows
       , countRows
       ) where

import           Control.Applicative (pure, liftA2)
import           Data.Profunctor     (lmap,Profunctor)
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator, orderAggregate)
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PackMap as PM

import qualified Opaleye.Column    as C
import qualified Opaleye.Order     as Ord
import qualified Opaleye.Select    as S
import qualified Opaleye.SqlTypes   as T
import qualified Opaleye.Join      as J

-- This page of Postgres documentation tell us what aggregate
-- functions are available
--
--   http://www.postgresql.org/docs/9.3/static/functions-aggregate.html

{-|
Given a 'S.Select' producing rows of type @a@ and an 'Aggregator' accepting rows of
type @a@, apply the aggregator to the select.

If you simply want to count the number of rows in a query you might
find the 'countRows' function more convenient.

If you want to use 'aggregate' with 'S.SelectArr's then you should
compose it with 'Opaleye.Lateral.laterally':

@
'Opaleye.Lateral.laterally' . 'aggregate' :: 'Aggregator' a b -> 'S.SelectArr' a b -> 'S.SelectArr' a b
@

Please note that when aggregating an empty query with no @GROUP BY@
clause, Opaleye's behaviour differs from Postgres's behaviour.
Postgres returns a single row whereas Opaleye returns zero rows.
Opaleye's behaviour is consistent with the meaning of aggregating
over groups of rows and Postgres's behaviour is inconsistent.  When a
query has zero rows it has zero groups, and thus zero rows in the
result of an aggregation.

-}
-- See 'Opaleye.Internal.Sql.aggregate' for details of how aggregating
-- by an empty query with no group by is handled.
aggregate :: Aggregator a b -> S.Select a -> S.Select b
aggregate agg q = Q.productQueryArr (A.aggregateU agg . Q.runSimpleQueryArr q)

-- | @aggregateLaterally :: Select (Aggregator () b) -> Select i b@
aggregateLaterally :: S.SelectArr i (Aggregator () b) -> S.SelectArr i b
aggregateLaterally q =
  Q.productQueryArr (\a ->
                        let (agg, pq, tag) = Q.runSimpleQueryArr q a
                        in A.aggregateU agg ((), pq, tag))

aggregateEasy :: D.Default AggregatorLaterally a b
              => S.SelectArr i a
              -> S.SelectArr i b
aggregateEasy = aggregateLaterally . fmap (runAggregatorLaterally D.def)


agg :: Aggregator a b -> a -> Aggregator () b
agg = flip (P.lmap . const)

data AggregatorLaterally a b = AggregatorLaterally { runAggregatorLaterally :: a -> Aggregator () b }

instance Functor (AggregatorLaterally a) where
  fmap f (AggregatorLaterally g) = AggregatorLaterally ((fmap . fmap) f g)

instance Applicative (AggregatorLaterally a) where
  pure a = AggregatorLaterally (pure (pure a))
  AggregatorLaterally f <*> AggregatorLaterally g = AggregatorLaterally (liftA2 (<*>) f g)

instance Profunctor (AggregatorLaterally) where
  dimap f g (AggregatorLaterally h) = AggregatorLaterally (P.dimap f (fmap g) h)

instance PP.ProductProfunctor (AggregatorLaterally) where
   purePP = pure
   (****) = (<*>)

instance a ~ b => D.Default AggregatorLaterally (Aggregator () b) a where
  def = AggregatorLaterally id

-- | Order the values within each aggregation in `Aggregator` using
-- the given ordering. This is only relevant for aggregations that
-- depend on the order they get their elements, like `arrayAgg` and
-- `stringAgg`.
--
-- Note that this orders all aggregations with the same ordering. If
-- you need different orderings for different aggregations, use
-- 'Opaleye.Internal.Aggregate.orderAggregate'.

aggregateOrdered  :: Ord.Order a -> Aggregator a b -> S.Select a -> S.Select b
aggregateOrdered o agg = aggregate (orderAggregate o agg)

-- | Aggregate only distinct values
distinctAggregator :: Aggregator a b -> Aggregator a b
distinctAggregator (A.Aggregator (PM.PackMap pm)) =
  A.Aggregator (PM.PackMap (\f c -> pm (f . P.first' (fmap (\(a,b,_) -> (a,b,HPQ.AggrDistinct)))) c))

-- | Group the aggregation by equality on the input to 'groupBy'.
groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = A.makeAggr' Nothing

-- | Sum all rows in a group.
--
-- WARNING! The type of this operation is wrong and will crash at
-- runtime when the argument is 'T.SqlInt4' or 'T.SqlInt8'.  For those
-- use 'sumInt4' or 'sumInt8' instead.
sum :: Aggregator (C.Column a) (C.Column a)
sum = A.makeAggr HPQ.AggrSum

sumInt4 :: Aggregator (C.Column T.SqlInt4) (C.Column T.SqlInt8)
sumInt4 = fmap C.unsafeCoerceColumn Opaleye.Aggregate.sum

sumInt8 :: Aggregator (C.Column T.SqlInt8) (C.Column T.SqlNumeric)
sumInt8 = fmap C.unsafeCoerceColumn Opaleye.Aggregate.sum

-- | Count the number of non-null rows in a group.
count :: Aggregator (C.Column a) (C.Column T.SqlInt8)
count = A.makeAggr HPQ.AggrCount

-- | Count the number of rows in a group.  This 'Aggregator' is named
-- @countStar@ after SQL's @COUNT(*)@ aggregation function.
countStar :: Aggregator a (C.Column T.SqlInt8)
countStar = lmap (const (0 :: C.Column T.SqlInt4)) count

-- | Average of a group
avg :: Aggregator (C.Column T.SqlFloat8) (C.Column T.SqlFloat8)
avg = A.makeAggr HPQ.AggrAvg

-- | Maximum of a group
max :: Ord.SqlOrd a => Aggregator (C.Column a) (C.Column a)
max = A.makeAggr HPQ.AggrMax

-- | Maximum of a group
min :: Ord.SqlOrd a => Aggregator (C.Column a) (C.Column a)
min = A.makeAggr HPQ.AggrMin

boolOr :: Aggregator (C.Column T.SqlBool) (C.Column T.SqlBool)
boolOr = A.makeAggr HPQ.AggrBoolOr

boolAnd :: Aggregator (C.Column T.SqlBool) (C.Column T.SqlBool)
boolAnd = A.makeAggr HPQ.AggrBoolAnd

arrayAgg :: Aggregator (C.Column a) (C.Column (T.SqlArray a))
arrayAgg = A.makeAggr HPQ.AggrArr

jsonAgg :: Aggregator (C.Column a) (C.Column T.SqlJson)
jsonAgg = A.makeAggr HPQ.JsonArr

stringAgg :: C.Column T.SqlText
          -> Aggregator (C.Column T.SqlText) (C.Column T.SqlText)
stringAgg = A.makeAggr' . Just . HPQ.AggrStringAggr . IC.unColumn

-- | Count the number of rows in a query.  This is different from
-- 'aggregate' 'count' because it always returns exactly one row, even
-- when the input query is empty.

-- This is currently implemented in a cheeky way with a LEFT JOIN.  If
-- there are any performance issues it could be rewritten to use an
-- SQL COUNT aggregation which groups by nothing.  This would require
-- changing the AST though, so I'm not too keen.
--
-- See https://github.com/tomjaguarpaw/haskell-opaleye/issues/162
countRows :: S.Select a -> S.Select (C.Column T.SqlInt8)
countRows = fmap (C.fromNullable 0)
            . fmap snd
            . (\q -> J.leftJoin (pure ())
                                (aggregate count q)
                                (const (T.sqlBool True)))
            . fmap (const (0 :: C.Column T.SqlInt4))
            --- ^^ The count aggregator requires an input of type
            -- 'Column a' rather than 'a' (I'm not sure if there's a
            -- good reason for this).  To deal with that restriction
            -- we just map a dummy integer value over it.
