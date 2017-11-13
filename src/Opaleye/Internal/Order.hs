module Opaleye.Internal.Order where

import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Data.Functor.Contravariant as C
import qualified Data.Functor.Contravariant.Divisible as Divisible
import qualified Data.Profunctor as P
import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import qualified Data.Void as Void

{-|
An `Order` @a@ represents a sort order and direction for the elements
of the type @a@. Multiple `Order`s can be composed with
`Data.Monoid.mappend` or @(\<\>)@ from "Data.Monoid".  If two rows are
equal according to the first `Order` in the @mappend@, the second is
used, and so on.
-}

-- Like the (columns -> RowParser haskells) field of QueryRunner this
-- type is "too big".  We never actually look at the 'a' (in the
-- QueryRunner case the 'colums') except to check the "structure".
-- This is so we can support a SumProfunctor instance.
newtype Order a = Order (a -> [(HPQ.OrderOp, HPQ.PrimExpr)])

instance C.Contravariant Order where
  contramap f (Order g) = Order (P.lmap f g)

instance S.Semigroup (Order a) where
  Order o <> Order o' = Order (o S.<> o')

instance M.Monoid (Order a) where
  mempty = Order M.mempty
  mappend = (S.<>)

instance Divisible.Divisible Order where
  divide f o o' = M.mappend (C.contramap (fst . f) o)
                            (C.contramap (snd . f) o')
  conquer = M.mempty

instance Divisible.Decidable Order where
  lose f = C.contramap f (Order Void.absurd)
  choose f (Order o) (Order o') = C.contramap f (Order (either o o'))

order :: HPQ.OrderOp -> (a -> C.Column b) -> Order a
order op f = Order (fmap (\column -> [(op, IC.unColumn column)]) f)

orderByU :: Order a -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
orderByU os (columns, primQ, t) = (columns, primQ', t)
  where primQ' = PQ.Order oExprs primQ
        oExprs = orderExprs columns os

orderExprs :: a -> Order a -> [HPQ.OrderExpr]
orderExprs x (Order os) = map (uncurry HPQ.OrderExpr) (os x)

limit' :: Int -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
limit' n (x, q, t) = (x, PQ.Limit (PQ.LimitOp n) q, t)

offset' :: Int -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
offset' n (x, q, t) = (x, PQ.Limit (PQ.OffsetOp n) q, t)
