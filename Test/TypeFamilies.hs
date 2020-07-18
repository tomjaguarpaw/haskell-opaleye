{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module TypeFamilies where

import Opaleye.Internal.TypeFamilies

data (:~) a b where
  Eq :: (:~) a a

-- If it compiles, it works
tests :: ()
tests = ()
  where _ = Eq :: a :~ (Pure :<$> Id :<| a :<| b)
        _ = Eq :: a :~ (Id :<| a)
        _ = Eq :: (a -> a) :~ (((->) :<$> Id :<*> Id) :<| a)
        _ = Eq :: (a -> b)
                      :~ (((->) :<$> Pure a :<*> Pure b) :<| c)
        _ = Eq :: Maybe a :~ ((Maybe :<$> Pure a) :<| b)
        _ = Eq :: Maybe a :~ ((Maybe :<$> Id) :<| a)
        _ = Eq :: a :~ (Pure a :<| b)
