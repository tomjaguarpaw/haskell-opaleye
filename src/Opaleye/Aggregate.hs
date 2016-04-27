-- | Perform aggregations on query results.
module Opaleye.Aggregate (module Opaleye.Aggregate, Aggregator) where

import           Control.Applicative (pure)
import           Data.Profunctor     (lmap)

import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator, orderAggregate)
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Opaleye.QueryArr  (Query)
import qualified Opaleye.Column    as C
import qualified Opaleye.Order     as Ord
import qualified Opaleye.PGTypes   as T
import qualified Opaleye.Join      as J

-- This page of Postgres documentation tell us what aggregate
-- functions are available
--
--   http://www.postgresql.org/docs/9.3/static/functions-aggregate.html

{-|
Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting rows of
type @a@, apply the aggregator to the results of the query.

Please note that when aggregating an empty query with no @GROUP BY@
clause, Opaleye's behaviour differs from Postgres's behaviour.
Postgres returns a single row whereas Opaleye returns zero rows.
(Opaleye's behaviour is consistent with the meaning of aggregating
over groups of rows and Postgres's behaviour is inconsistent.  When a
query has zero rows it has zero groups, and thus zero rows in the
result of an aggregation.)

If you simply want to count the number of rows in a query you might
find the 'countRows' function more convenient.

-}
aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = Q.simpleQueryArr (A.aggregateU agg . Q.runSimpleQueryArr q)

-- | Order the values within each aggregation in `Aggregator` using
-- the given ordering. This is only relevant for aggregations that
-- depend on the order they get their elements, like `arrayAgg` and
-- `stringAgg`.
--
-- Note that this orders all aggregations with the same ordering. If
-- you need different orderings for different aggregations, use
-- 'Opaleye.Internal.Aggregate.orderAggregate'.

aggregateOrdered  :: Ord.Order a -> Aggregator a b -> Query a -> Query b
aggregateOrdered o agg = aggregate (orderAggregate o agg)

-- | Group the aggregation by equality on the input to 'groupBy'.
groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = A.makeAggr' Nothing

-- | Sum all rows in a group.
sum :: Aggregator (C.Column a) (C.Column a)
sum = A.makeAggr HPQ.AggrSum

-- | Count the number of non-null rows in a group.
count :: Aggregator (C.Column a) (C.Column T.PGInt8)
count = A.makeAggr HPQ.AggrCount

-- | Count the number of rows in a group.  This 'Aggregator' is named
-- @countStar@ after SQL's @COUNT(*)@ aggregation function.
countStar :: Aggregator a (C.Column T.PGInt8)
countStar = lmap (const (0 :: C.Column T.PGInt4)) count

-- | Average of a group
avg :: Aggregator (C.Column T.PGFloat8) (C.Column T.PGFloat8)
avg = A.makeAggr HPQ.AggrAvg

-- | Maximum of a group
max :: Ord.PGOrd a => Aggregator (C.Column a) (C.Column a)
max = A.makeAggr HPQ.AggrMax

-- | Maximum of a group
min :: Ord.PGOrd a => Aggregator (C.Column a) (C.Column a)
min = A.makeAggr HPQ.AggrMin

boolOr :: Aggregator (C.Column T.PGBool) (C.Column T.PGBool)
boolOr = A.makeAggr HPQ.AggrBoolOr

boolAnd :: Aggregator (C.Column T.PGBool) (C.Column T.PGBool)
boolAnd = A.makeAggr HPQ.AggrBoolAnd

arrayAgg :: Aggregator (C.Column a) (C.Column (T.PGArray a))
arrayAgg = A.makeAggr HPQ.AggrArr

stringAgg :: C.Column T.PGText -> Aggregator (C.Column T.PGText) (C.Column T.PGText)
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
countRows :: Query a -> Query (C.Column T.PGInt8)
countRows = fmap (C.fromNullable 0)
            . fmap snd
            . (\q -> J.leftJoin (pure ())
                                (aggregate count q)
                                (const (T.pgBool True)))
            . fmap (const (0 :: C.Column T.PGInt4))
            --- ^^ The count aggregator requires an input of type
            -- 'Column a' rather than 'a' (I'm not sure if there's a
            -- good reason for this).  To deal with that restriction
            -- we just map a dummy integer value over it.
