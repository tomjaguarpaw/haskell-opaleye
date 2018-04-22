{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Values where

import qualified Opaleye.Internal.QueryArr as Q
import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.Values as V
import qualified Opaleye.Internal.Unpackspec as U

import           Data.Profunctor.Product.Default (Default, def)

-- | 'values' implements Postgres's @VALUES@ construct and allows you
-- to create a query that consists of the given rows.
--
-- Example type specialization:
--
-- @
-- values :: [(Column a, Column b)] -> Query (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- queryTable :: [Foo (Column a) (Column b) (Column c)] -> Query (Foo (Column a) (Column b) (Column c))
-- @
values :: (Default V.Valuesspec fields fields,
           Default U.Unpackspec fields fields) =>
          [fields] -> Q.Query fields
values = valuesExplicit def def

valuesExplicit :: U.Unpackspec fields fields'
               -> V.Valuesspec fields fields'
               -> [fields] -> Query fields'
valuesExplicit unpack valuesspec fields =
  Q.simpleQueryArr (V.valuesU unpack valuesspec fields)
