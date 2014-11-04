module Opaleye.Internal.Order where

import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Tag as T

import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Data.Functor.Contravariant as C
import qualified Data.Profunctor as P
import qualified Data.Monoid as M

-- FIXME: make capitilisation of Specs consistent with UnpackSpec
data SingleOrderSpec a = SingleOrderSpec PQ.OrderOp (a -> PQ.PrimExpr)

instance C.Contravariant SingleOrderSpec where
  contramap f (SingleOrderSpec op g) = SingleOrderSpec op (P.lmap f g)

newtype OrderSpec a = OrderSpec [SingleOrderSpec a]

instance C.Contravariant OrderSpec where
  contramap f (OrderSpec xs) = OrderSpec (fmap (C.contramap f) xs)

instance M.Monoid (OrderSpec a) where
  mempty = OrderSpec M.mempty
  OrderSpec o `mappend` OrderSpec o' = OrderSpec (o `M.mappend` o')

orderSpec :: PQ.OrderOp -> (a -> C.Column b) -> OrderSpec a
orderSpec op f = C.contramap f (OrderSpec [SingleOrderSpec op C.unColumn])

orderByU :: OrderSpec a -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
orderByU os (columns, primQ, t) = (columns, primQ', t)
  where primQ' = PQ.Special (PQ.Order orderExprs) primQ
        OrderSpec sos = os
        orderExprs = map (\(SingleOrderSpec op f)
                          -> PQ.OrderExpr op (f columns)) sos
