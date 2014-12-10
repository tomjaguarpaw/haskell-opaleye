{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Distinct (module Opaleye.Distinct, distinctExplicit)
       where

import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.Distinct (distinctExplicit, Distinctspec)

import qualified Data.Profunctor.Product.Default as D

-- | Remove duplicate items from the query result.
--
-- Example type specialization:
--
-- @
-- distinct :: Query (Column a, Column b) -> Query (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- distinct :: Query (Foo (Column a) (Column b) (Column c)) -> Query (Foo (Column a) (Column b) (Column c))
-- @
distinct :: D.Default Distinctspec columns columns =>
            Query columns -> Query columns
distinct = distinctExplicit D.def
