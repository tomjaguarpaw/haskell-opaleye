{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Opaleye.Test.TraverseA where

import Control.Arrow

-- I first learned this from Alexis King at
-- https://github.com/tomjaguarpaw/Arrows2/issues/3#issuecomment-561973678

data Traversal a r b
  = Done b
  | Yield a !(r -> Traversal a r b)

instance Functor (Traversal a r) where
  fmap f = \case
    Done x -> Done (f x)
    Yield v k -> Yield v (fmap f . k)

instance Applicative (Traversal a r) where
  pure = Done
  tf <*> tx = case tf of
    Done f -> fmap f tx
    Yield v k -> Yield v ((<*> tx) . k)

traversal :: Traversable t => t a -> Traversal a b (t b)
traversal = traverse (flip Yield Done)

traverseA :: (ArrowChoice arr, Traversable t)
          => arr (e, a) b -> arr (e, t a) (t b)
traverseA f = second (arr traversal) >>> go where
  go = proc (e, as) -> case as of
    Done bs -> returnA -< bs
    Yield a k -> do
      b <- f -< (e, a)
      go -< (e, k b)

traverseA1 :: (ArrowChoice arr, Traversable t)
           => arr a b -> arr (t a) (t b)
traverseA1 f = arr (\x -> ((), x)) >>> traverseA (arr snd >>> f)
