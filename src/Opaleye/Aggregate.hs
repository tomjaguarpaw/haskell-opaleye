{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Perform aggregations on query results.
module Opaleye.Aggregate (module Opaleye.Aggregate, Aggregator) where

import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator)
import qualified Opaleye.Internal.Column as IC
import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Column as C
import qualified Opaleye.Order as Ord
import qualified Opaleye.PGTypes as T
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- This page of Postgres documentation tell us what aggregate
-- functions are available
--
--   http://www.postgresql.org/docs/9.3/static/functions-aggregate.html

{-|
Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting rows of
type @a@, apply the aggregator to the results of the query.

-}
aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = Q.simpleQueryArr (A.aggregateU agg . Q.runSimpleQueryArr q)

-- | Group the aggregation by equality on the input to 'groupBy'.
groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = A.makeAggr' Nothing

-- | Sum all rows in a group.
sum :: Sum a b => Aggregator (C.Column a) (C.Column b)
sum = A.makeAggr HPQ.AggrSum

-- | Count the number of non-null rows in a group.
count :: Aggregator (C.Column a) (C.Column T.PGInt8)
count = A.makeAggr HPQ.AggrCount

-- | Average of a group
avg :: Avg a b => Aggregator (C.Column a) (C.Column b)
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

stringAgg :: StringAgg a => C.Column a -> Aggregator (C.Column a) (C.Column a)
stringAgg = A.makeAggr' . Just . HPQ.AggrStringAggr . IC.unColumn

stddevPop :: StddevVar a b => Aggregator (C.Column a) (C.Column b)
stddevPop = A.makeAggr HPQ.AggrStddevPop

stddevSamp :: StddevVar a b => Aggregator (C.Column a) (C.Column b)
stddevSamp = A.makeAggr HPQ.AggrStddevSamp

varPop :: StddevVar a b => Aggregator (C.Column a) (C.Column b)
varPop = A.makeAggr HPQ.AggrVarPop

varSamp :: StddevVar a b => Aggregator (C.Column a) (C.Column b)
varSamp = A.makeAggr HPQ.AggrVarSamp

class StringAgg a where
instance StringAgg T.PGText
instance StringAgg T.PGCitext
instance StringAgg T.PGBytea

class Avg a b | a -> b where
instance Avg T.PGInt2 T.PGNumeric
instance Avg T.PGInt4 T.PGNumeric
instance Avg T.PGInt8 T.PGNumeric
instance Avg T.PGFloat4 T.PGFloat4
instance Avg T.PGFloat8 T.PGFloat8

class Sum a b | a -> b where
instance Sum T.PGInt2 T.PGInt8
instance Sum T.PGInt4 T.PGInt8
instance Sum T.PGInt8 T.PGNumeric
instance Sum T.PGFloat4 T.PGFloat4
instance Sum T.PGFloat8 T.PGFloat8
instance Sum T.PGNumeric T.PGNumeric

class StddevVar a b | a -> b where
instance StddevVar T.PGFloat4 T.PGFloat8
instance StddevVar T.PGFloat8 T.PGFloat8
instance StddevVar T.PGNumeric T.PGNumeric
instance StddevVar T.PGInt2 T.PGNumeric
instance StddevVar T.PGInt4 T.PGNumeric
instance StddevVar T.PGInt8 T.PGNumeric
