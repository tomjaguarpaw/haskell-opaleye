module Opaleye.Aggregate where

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.Tag as T
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import qualified Opaleye.Column as C

import qualified Database.HaskellDB.PrimQuery as PQ

import qualified Control.Monad.Trans.State as S
import           Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

newtype Aggregator a b = Aggregator
                         (PM.PackMap (PQ.PrimExpr, Maybe PQ.AggrOp) PQ.PrimExpr
                                     a b)

groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = makeAggr' Nothing

sum :: Aggregator (C.Column a) (C.Column a)
sum = makeAggr PQ.AggrSum

count :: Aggregator (C.Column a) (C.Column a)
count = makeAggr PQ.AggrCount

makeAggr' :: Maybe PQ.AggrOp -> Aggregator (C.Column a) (C.Column a)
makeAggr' m = Aggregator (PM.PackMap
                          (\f (C.Column e) -> fmap C.Column (f (e, m))))

makeAggr :: PQ.AggrOp -> Aggregator (C.Column a) (C.Column a)
makeAggr = makeAggr' . Just

runAggregator :: Applicative f => Aggregator a b
              -> ((PQ.PrimExpr, Maybe PQ.AggrOp) -> f PQ.PrimExpr) -> a -> f b
runAggregator (Aggregator a) = PM.packmap a

aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = Q.simpleQueryArr (aggregateU agg . Q.runSimpleQueryArr q)

-- FIXME: duplication with distinctU
-- FIXME: probably need to use the tag
aggregateU :: Aggregator a b
           -> (a, PQ.PrimQuery, T.Tag) -> (b, PQ.PrimQuery, T.Tag)
aggregateU agg (c0, primQ, t0) = (c1, primQ', t0)
  where f :: (PQ.PrimExpr, Maybe PQ.AggrOp)
          -> S.State ([(String, PQ.PrimExpr)], Int) PQ.PrimExpr
        f (pe, maggrop) = do
          (projPEs, i) <- S.get
          let s = "result" ++ show i
          let aggrpe = case maggrop of
                Nothing -> id
                Just aggrop -> PQ.AggrExpr aggrop
          S.put (projPEs ++ [(s, aggrpe pe)], i+1)
          return (PQ.AttrExpr s)

        (c1, (projPEs', _)) =
          S.runState (runAggregator agg f c0) ([], 0)

        primQ' = PQ.Project projPEs' primQ

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

-- }
