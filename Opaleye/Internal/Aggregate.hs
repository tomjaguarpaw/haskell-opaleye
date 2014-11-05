module Opaleye.Internal.Aggregate where

import           Control.Applicative (Applicative, pure, (<*>))
import qualified Control.Monad.Trans.State as S

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Column as C

import qualified Database.HaskellDB.PrimQuery as HPQ

newtype Aggregator a b = Aggregator
                         (PM.PackMap (HPQ.PrimExpr, Maybe HPQ.AggrOp) HPQ.PrimExpr
                                     a b)

makeAggr' :: Maybe HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr' m = Aggregator (PM.PackMap
                          (\f (C.Column e) -> fmap C.Column (f (e, m))))

makeAggr :: HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr = makeAggr' . Just

runAggregator :: Applicative f => Aggregator a b
              -> ((HPQ.PrimExpr, Maybe HPQ.AggrOp) -> f HPQ.PrimExpr) -> a -> f b
runAggregator (Aggregator a) = PM.packmap a

-- FIXME: duplication with distinctU
aggregateU :: Aggregator a b
           -> (a, PQ.PrimQuery, T.Tag) -> (b, PQ.PrimQuery, T.Tag)
aggregateU agg (c0, primQ, t0) = (c1, primQ', T.next t0)
  where f :: (HPQ.PrimExpr, Maybe HPQ.AggrOp)
          -> S.State ([(String, Maybe HPQ.AggrOp, HPQ.PrimExpr)], Int) HPQ.PrimExpr
        f (pe, maggrop) = do
          (projPEs, i) <- S.get
          let s = T.tagWith t0 ("result" ++ show i)
          S.put (projPEs ++ [(s, maggrop, pe)], i+1)
          return (HPQ.AttrExpr s)

        (c1, (projPEs', _)) =
          S.runState (runAggregator agg f c0) ([], 0)

        primQ' = PQ.Aggregate projPEs' primQ

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
