{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Internal.Rebind where

import Data.Profunctor.Product.Default (Default, def)
import Opaleye.Internal.Unpackspec (Unpackspec, runUnpackspec)
import Opaleye.Internal.QueryArr (selectArr, SelectArr)
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Tag as Tag

rebind :: Default Unpackspec a a => SelectArr a a
rebind = rebindExplicit def

rebindExplicit :: Unpackspec a b -> SelectArr a b
rebindExplicit = rebindExplicitPrefix "rebind"

rebindExplicitPrefix :: String -> Unpackspec a b -> SelectArr a b
rebindExplicitPrefix prefix u = selectArr $ do
  tag <- Tag.fresh
  pure $ \a ->
    let (b, bindings) = PM.run (runUnpackspec u (PM.extractAttr prefix tag) a)
    in (b, PQ.aRebind bindings)
