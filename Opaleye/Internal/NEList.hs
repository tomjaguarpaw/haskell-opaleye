module Opaleye.Internal.NEList where

data NEList a = NEList a [a] deriving Show

singleton :: a -> NEList a
singleton = flip NEList []

toList :: NEList a -> [a]
toList (NEList a as) = a:as

instance Functor NEList where
  fmap f (NEList a as) = NEList (f a) (fmap f as)
