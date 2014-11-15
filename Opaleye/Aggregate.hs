-- | Perform aggregations on query results.
module Opaleye.Aggregate (module Opaleye.Aggregate, Aggregator) where

import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator)
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import qualified Opaleye.Column as C

import qualified Database.HaskellDB.PrimQuery as HPQ

-- | Group the aggregation by equality on the input to 'groupBy'.
groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = A.makeAggr' Nothing

-- | Sum all rows in a group.
sum :: Aggregator (C.Column a) (C.Column a)
sum = A.makeAggr HPQ.AggrSum

-- TODO: We have to decide what is the most appropriate return type
-- for the count aggregator.  In Postgres it returns a 64 bit integer,
-- but making this explicit will imply a lot of annoying fiddling on
-- the part of our users.  Can we get away with just saying 'Integer'
-- here?
-- | Count the number of non-null rows in a group.
count :: Aggregator (C.Column a) (C.Column Integer)
count = A.makeAggr HPQ.AggrCount

{-|
Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting rows of
type @a@, apply the aggregator to the results of the query.
-}
aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = Q.simpleQueryArr (A.aggregateU agg . Q.runSimpleQueryArr q)
