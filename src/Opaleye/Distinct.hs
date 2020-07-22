{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Distinct (module Opaleye.Distinct, distinctExplicit)
       where

import           Opaleye.Select (Select)
import           Opaleye.Internal.Distinct (distinctExplicit, Distinctspec)

import qualified Data.Profunctor.Product.Default as D

-- | Remove duplicate rows from the 'Select'.
--
-- Example type specialization:
--
-- @
-- distinct :: Select (Field a, Field b) -> Select (Field a, Field b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- distinct :: Select (Foo (Field a) (Field b) (Field c)) -> Select (Foo (Field a) (Field b) (Field c))
-- @
--
-- By design there is no @distinct@ function of type @SelectArr a b ->
-- SelectArr a b@.  Such a function would allow violation of SQL's
-- scoping rules and lead to invalid queries.
distinct :: D.Default Distinctspec fields fields =>
            Select fields -> Select fields
distinct = distinctExplicit D.def
