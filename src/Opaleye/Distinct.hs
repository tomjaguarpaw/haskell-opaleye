{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Distinct (module Opaleye.Distinct, distinctExplicit)
       where

import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.Distinct (distinctExplicit, Distinctspec)

import qualified Data.Profunctor.Product.Default as D

distinct :: D.Default Distinctspec columns columns =>
            Query columns -> Query columns
distinct = distinctExplicit D.def
