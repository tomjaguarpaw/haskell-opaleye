{-# LANGUAGE Arrows #-}

module Bug where

import qualified Data.Profunctor.Product as PP (pT62)

chooseChoice :: (a -> i) -> f i -> f a
chooseChoice choose fi = _ $ proc a -> do
  case choose a of
    i  -> fi -< i
