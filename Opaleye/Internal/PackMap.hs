{-# LANGUAGE Rank2Types #-}

module Opaleye.Internal.PackMap where

import           Control.Applicative (Applicative, pure, (<*>), liftA2)
import qualified Control.Monad.Trans.State as S
import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import qualified Data.Functor.Identity as I

-- This is rather like a Control.Lens.Traversal with the type
-- parameters switched but I'm not sure if it should be required to
-- obey the same laws.
--
-- TODO: We should actually generalise this to
--
-- data LensLike f a b s t = LensLike ((a -> f b) -> s -> f t)
--
-- i.e. a wrapped, argument-flipped Control.Lens.LensLike
--
-- This would allow us to do the Profunctor and ProductProfunctor
-- instances (requiring just Functor f and Applicative f respectively)
-- and share them between many different restrictions of f.  For
-- example, TableColumnMaker is like a Setter so we would restrict f
-- to the Distributive case.  There are probably other examples but I
-- haven't looked yet.
data PackMap a b s t = PackMap (Applicative f =>
                                (a -> f b) -> s -> f t)

packmap :: Applicative f => PackMap a b s t -> (a -> f b) -> s -> f t
packmap (PackMap f) = f

over :: PackMap a b s t -> (a -> b) -> s -> t
over p f = I.runIdentity . packmap p (I.Identity . f)


type PM a = S.State (a, Int)

new :: PM a String
new = do
  (a, i) <- S.get
  S.put (a, i + 1)
  return (show i)

write :: a -> PM [a] ()
write a = do
  (as, i) <- S.get
  S.put (as ++ [a], i)

run :: PM [a] r -> (r, [a])
run m = (r, as)
  where (r, (as, _)) = S.runState m ([], 0)

-- {

-- Boilerplate instance definitions.  There's no choice here apart
-- from the order in which the applicative is applied.

instance Functor (PackMap a b s) where
  fmap f (PackMap g) = PackMap ((fmap . fmap . fmap) f g)

instance Applicative (PackMap a b s) where
  pure x = PackMap (pure (pure (pure x)))
  PackMap f <*> PackMap x = PackMap (liftA2 (liftA2 (<*>)) f x)

instance Profunctor (PackMap a b) where
  dimap f g (PackMap q) = PackMap (fmap (dimap f (fmap g)) q)

instance ProductProfunctor (PackMap a b) where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

-- }
