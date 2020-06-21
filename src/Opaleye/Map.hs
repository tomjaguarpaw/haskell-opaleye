{-# LANGUAGE TypeFamilies #-}

-- | Do not use. This module will be deprecated in 0.7.

module Opaleye.Map where

type family Map f x

type instance Map f (a1, a2)
  = (Map f a1, Map f a2)
type instance Map f (a1, a2, a3)
  = (Map f a1, Map f a2, Map f a3)
type instance Map f (a1, a2, a3, a4)
  = (Map f a1, Map f a2, Map f a3, Map f a4)
type instance Map f (a1, a2, a3, a4, a5)
  = (Map f a1, Map f a2, Map f a3, Map f a4, Map f a5)
type instance Map f (a1, a2, a3, a4, a5, a6)
  = (Map f a1, Map f a2, Map f a3, Map f a4, Map f a5, Map f a6)
type instance Map f (a1, a2, a3, a4, a5, a6, a7)
  = (Map f a1, Map f a2, Map f a3, Map f a4, Map f a5, Map f a6,
     Map f a7)
type instance Map f (a1, a2, a3, a4, a5, a6, a7, a8)
  = (Map f a1, Map f a2, Map f a3, Map f a4, Map f a5, Map f a6,
     Map f a7, Map f a8)
