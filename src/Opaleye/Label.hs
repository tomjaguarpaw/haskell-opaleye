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
label' l = Q.selectArr f where
  f = pure (\() -> ((), PQ.aLabel l))

{-# DEPRECATED label "Will be removed in version 0.11.  Use 'label\'' instead." #-}
label :: String -> S.SelectArr a b -> S.SelectArr a b
label l s = proc a -> do
  b <- s -< a
  label' l -< ()
  returnA -< b
