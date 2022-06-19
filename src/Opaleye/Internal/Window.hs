module Opaleye.Internal.Window where

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Opaleye.Internal.Aggregate as A
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.Order as O

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ


newtype Window a =
  Window (PM.PackMap (HPQ.WndwOp, Partition) HPQ.PrimExpr () a)


instance Functor Window where
  fmap f (Window g) = Window (fmap f g)


instance Applicative Window where
  pure = Window . pure
  Window f <*> Window x = Window (f <*> x)


runWindow :: Applicative f
  => Window a -> ((HPQ.WndwOp, Partition) -> f HPQ.PrimExpr) -> f a
runWindow (Window a) f = PM.traversePM a f ()


extractWindowFields
  :: T.Tag
  -> (HPQ.WndwOp, Partition)
  -> PM.PM (PQ.Bindings (HPQ.WndwOp, HPQ.Partition)) HPQ.PrimExpr
extractWindowFields tag (op, Partition ps os) = do
  i <- PM.new
  let symbol = HPQ.Symbol ("window" ++ i) tag
  PM.write (symbol, (op, (HPQ.Partition ps os)))
  pure (HPQ.AttrExpr symbol)


window :: Q.Select (Window a) -> Q.Select a
window q = Q.productQueryArr $ do
  (wndw, primQ) <- Q.runSimpleQueryArr' q ()
  tag <- T.fresh
  let
    (a, bindings) = PM.run (runWindow wndw (extractWindowFields tag))
  pure (a, PQ.Window bindings primQ)


makeWndw :: HPQ.WndwOp -> Window (C.Field_ n a)
makeWndw op = Window (PM.PackMap (\f _ -> C.Column <$> f (op, mempty)))


cumulative :: A.Aggregator a b -> a -> Window b
cumulative (A.Aggregator (PM.PackMap pm)) a = Window $ PM.PackMap $ \f _ ->
  pm (\(mop, expr) -> case mop of
         Nothing -> pure expr
         Just (op, _, _) -> f (HPQ.WndwAggregate op expr, mempty)) a


over :: Window a -> Partition -> Window a
over (Window (PM.PackMap pm)) partition =
  Window $ PM.PackMap $ \f -> pm $ \(op, partition') ->
    f (op, partition' <> partition)
infixl 1 `over`


data Partition = Partition ![HPQ.PrimExpr] ![HPQ.OrderExpr]


instance Semigroup Partition where
  Partition ps os <> Partition ps' os' = Partition (ps <> ps') (os <> os')


instance Monoid Partition where
  mempty = Partition [] []


partitionBy :: C.Field_ n a -> Partition
partitionBy (C.Column expr) = Partition [expr] []


orderBy :: a -> O.Order a -> Partition
orderBy a ordering = Partition [] (O.orderExprs a ordering)
