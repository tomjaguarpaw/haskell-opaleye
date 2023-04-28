{-# LANGUAGE DataKinds #-}

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
       , aggregateOrdered
       , distinctAggregator
       , filterWhere
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
       , arrayAgg_
       , jsonAgg
       , stringAgg
       -- * Counting rows
       , countRows
       ) where

import           Control.Applicative (pure)
import           Data.Profunctor     (lmap)
import qualified Data.Profunctor as P

import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator, orderAggregate)
import qualified Opaleye.Internal.Column as IC
import           Opaleye.Internal.MaybeFields (MaybeFields (MaybeFields))
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.Operators as O
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.Tag as Tag

import qualified Opaleye.Field     as F
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
aggregate agg q = Q.productQueryArr $ do
  (a, pq) <- Q.runSimpleSelect q
  t <- Tag.fresh
  pure (A.aggregateU agg (a, pq, t))

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
  A.Aggregator (PM.PackMap (\f c -> pm (f . P.first' setDistinct) c))
  where
    setDistinct HPQ.GroupBy = HPQ.GroupBy
    setDistinct aggr =
      aggr
        { HPQ.aggrDistinct = HPQ.AggrDistinct
        }

-- | Aggregate only rows matching the given predicate
filterWhere
  :: (a -> F.Field T.SqlBool)
  -> Aggregator a b
  -> Aggregator a (MaybeFields b)
filterWhere = A.filterWhereInternal (MaybeFields . O.not . F.isNull)

-- | Group the aggregation by equality on the input to 'groupBy'.
groupBy :: Aggregator (F.Field_ n a) (F.Field_ n a)
groupBy = A.makeAggr' Nothing

-- | Sum all rows in a group.
--
-- WARNING! The type of this operation is wrong and will crash at
-- runtime when the argument is 'T.SqlInt4' or 'T.SqlInt8'.  For those
-- use 'sumInt4' or 'sumInt8' instead.
sum :: Aggregator (F.Field a) (F.Field a)
sum = A.unsafeSum

sumInt4 :: Aggregator (F.Field T.SqlInt4) (F.Field T.SqlInt8)
sumInt4 = fmap F.unsafeCoerceField Opaleye.Aggregate.sum

sumInt8 :: Aggregator (F.Field T.SqlInt8) (F.Field T.SqlNumeric)
sumInt8 = fmap F.unsafeCoerceField Opaleye.Aggregate.sum

-- | Count the number of non-null rows in a group.
count :: Aggregator (F.Field a) (F.Field T.SqlInt8)
count = A.makeAggr HPQ.AggrCount

-- | Count the number of rows in a group.  This 'Aggregator' is named
-- @countStar@ after SQL's @COUNT(*)@ aggregation function.
countStar :: Aggregator a (F.Field T.SqlInt8)
countStar = lmap (const (0 :: F.Field T.SqlInt4)) count

-- | Average of a group
avg :: Aggregator (F.Field T.SqlFloat8) (F.Field T.SqlFloat8)
avg = A.unsafeAvg

-- | Maximum of a group
max :: Ord.SqlOrd a => Aggregator (F.Field a) (F.Field a)
max = A.unsafeMax

-- | Maximum of a group
min :: Ord.SqlOrd a => Aggregator (F.Field a) (F.Field a)
min = A.unsafeMin

boolOr :: Aggregator (F.Field T.SqlBool) (F.Field T.SqlBool)
boolOr = A.makeAggr HPQ.AggrBoolOr

boolAnd :: Aggregator (F.Field T.SqlBool) (F.Field T.SqlBool)
boolAnd = A.makeAggr HPQ.AggrBoolAnd

arrayAgg :: Aggregator (F.Field a) (F.Field (T.SqlArray a))
arrayAgg = P.dimap F.unsafeCoerceField F.unsafeCoerceField arrayAgg_

arrayAgg_ :: Aggregator (F.Field_ n a) (F.Field (T.SqlArray_ n a))
arrayAgg_ = A.makeAggr HPQ.AggrArr

{-|
Aggregates values as a JSON array. An example usage:

@
import qualified Opaleye as O

O.aggregate O.jsonAgg $ do
    (firstCol, secondCol) <- O.selectTable table6
    return
      . O.jsonBuildObject
      $ O.jsonBuildObjectField "summary" firstCol
        <> O.jsonBuildObjectField "details" secondCol
@

The above query, when executed, will return JSON of the following form from postgres:

@"[{\\"summary\\" : \\"xy\\", \\"details\\" : \\"a\\"}, {\\"summary\\" : \\"z\\", \\"details\\" : \\"a\\"}, {\\"summary\\" : \\"more text\\", \\"details\\" : \\"a\\"}]"@
-}
jsonAgg :: Aggregator (F.Field a) (F.Field T.SqlJson)
jsonAgg = A.makeAggr HPQ.JsonArr

stringAgg :: F.Field T.SqlText
          -> Aggregator (F.Field T.SqlText) (F.Field T.SqlText)
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
countRows :: S.Select a -> S.Select (F.Field T.SqlInt8)
countRows = fmap (F.fromNullable 0)
            . fmap snd
            . (\q -> J.leftJoin (pure ())
                                (aggregate count q)
                                (const (T.sqlBool True)))
            . fmap (const (0 :: F.Field T.SqlInt4))
            --- ^^ The count aggregator requires an input of type
            -- 'Column a' rather than 'a' (I'm not sure if there's a
            -- good reason for this).  To deal with that restriction
            -- we just map a dummy integer value over it.
