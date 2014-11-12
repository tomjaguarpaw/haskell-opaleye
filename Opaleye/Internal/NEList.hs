module Opaleye.Internal.NEList where

import Control.Applicative (Applicative(..))


data NEList a = NEList a [a] deriving Show

singleton :: a -> NEList a
singleton = flip NEList []

toList :: NEList a -> [a]
toList (NEList a as) = a:as

neCat :: NEList a -> NEList a -> NEList a
neCat (NEList a as) bs = NEList a (as ++ toList bs)

instance Functor NEList where
  fmap f (NEList a as) = NEList (f a) (fmap f as)

instance Applicative NEList where
  pure = flip NEList []
  f <*> x = let (y:ys) = toList f <*> toList x
            in NEList y ys

instance Monad NEList where
  return = flip NEList []
  NEList a as >>= f = NEList b (bs ++ (as >>= (toList . f)))
    where NEList b bs = f a
