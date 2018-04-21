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
-- distinct :: Select (Column a, Column b) -> Select (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- distinct :: Select (Foo (Column a) (Column b) (Column c)) -> Select (Foo (Column a) (Column b) (Column c))
-- @
--
-- By design there is no @distinct@ function of type @SelectArr a b ->
-- SelectArr a b@.  Such a function would allow violation of SQL's
-- scoping rules and lead to invalid queries.
distinct :: D.Default Distinctspec columns columns =>
            Select columns -> Select columns
distinct = distinctExplicit D.def
