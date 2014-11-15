module Opaleye.Internal.Aggregate where

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.Column as C

import qualified Database.HaskellDB.PrimQuery as HPQ

{-|
An 'Aggregator' takes a collection of rows of type @a@, groups
them, and transforms each group into a single row of type @b@. This
corresponds to aggregators using @GROUP BY@ in SQL.
-}
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

aggregateU :: Aggregator a b
           -> (a, PQ.PrimQuery, T.Tag) -> (b, PQ.PrimQuery, T.Tag)
aggregateU agg (c0, primQ, t0) = (c1, primQ', T.next t0)
  where (c1, projPEs) =
          PM.run (runAggregator agg (extractAggregateFields t0) c0)

        primQ' = PQ.Aggregate projPEs primQ

extractAggregateFields :: T.Tag -> (HPQ.PrimExpr, Maybe HPQ.AggrOp)
      -> PM.PM [(String, Maybe HPQ.AggrOp, HPQ.PrimExpr)] HPQ.PrimExpr
extractAggregateFields tag (pe, maggrop) = do
  i <- PM.new
  let s = T.tagWith tag ("result" ++ i)
  PM.write (s, maggrop, pe)
  return (HPQ.AttrExpr s)

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
