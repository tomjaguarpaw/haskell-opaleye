module Opaleye.Aggregate where

import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator)
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import qualified Opaleye.Column as C

import qualified Database.HaskellDB.PrimQuery as PQ

groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = A.makeAggr' Nothing

sum :: Aggregator (C.Column a) (C.Column a)
sum = A.makeAggr PQ.AggrSum

count :: Aggregator (C.Column a) (C.Column a)
count = A.makeAggr PQ.AggrCount

aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = Q.simpleQueryArr (A.aggregateU agg . Q.runSimpleQueryArr q)
