module Opaleye.Internal.NEList where

data NEList a = NEList a [a] deriving Show

singleton :: a -> NEList a
singleton = flip NEList []

toList :: NEList a -> [a]
toList (NEList a as) = a:as

neCat :: NEList a -> NEList a -> NEList a
neCat (NEList a as) bs = NEList a (as ++ toList bs)

instance Functor NEList where
  fmap f (NEList a as) = NEList (f a) (fmap f as)

instance Monad NEList where
  return = flip NEList []
  NEList a as >>= f = NEList b (bs ++ (as >>= (toList . f)))
    where NEList b bs = f a
