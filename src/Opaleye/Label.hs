{-# LANGUAGE Arrows #-}

module Opaleye.Label (
  label',
  -- * Deprecated
  label
  ) where

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Select            as S

import           Control.Arrow (returnA)

-- | Add a commented label to the generated SQL.
label' :: String -> S.Select ()
label' l = Q.QueryArr f where
  f () = pure ((), PQ.PrimQueryArr $ \_ primQ -> PQ.Label l primQ)

-- | Will be deprecated in version 0.10.  Use 'label\'' instead.
label :: String -> S.SelectArr a b -> S.SelectArr a b
label l s = proc a -> do
  b <- s -< a
  label' l -< ()
  returnA -< b
