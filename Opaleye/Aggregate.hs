module Opaleye.Aggregate where

import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator)
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import qualified Opaleye.Column as C

import qualified Database.HaskellDB.PrimQuery as HPQ

groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = A.makeAggr' Nothing

sum :: Aggregator (C.Column a) (C.Column a)
sum = A.makeAggr HPQ.AggrSum

-- TODO: We have to decide what is the most appropriate return type
-- for the count aggregator.  In Postgres it returns a 64 bit integer,
-- but making this explicit will imply a lot of annoying fiddling on
-- the part of our users.  Can we get away with just saying 'Integer'
-- here?
count :: Aggregator (C.Column a) (C.Column Integer)
count = A.makeAggr HPQ.AggrCount

aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = Q.simpleQueryArr (A.aggregateU agg . Q.runSimpleQueryArr q)
