{-# LANGUAGE FlexibleContexts #-}

module Opaleye.SQLite.Values where

import qualified Opaleye.SQLite.Internal.QueryArr as Q
import           Opaleye.SQLite.QueryArr (Query)
import           Opaleye.SQLite.Internal.Values as V
import qualified Opaleye.SQLite.Internal.Unpackspec as U

import           Data.Profunctor.Product.Default (Default, def)

-- | Example type specialization:
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
values :: (Default V.Valuesspec columns columns,
           Default U.Unpackspec columns columns) =>
          [columns] -> Q.Query columns
values = valuesExplicit def def

valuesExplicit :: U.Unpackspec columns columns'
               -> V.Valuesspec columns columns'
               -> [columns] -> Query columns'
valuesExplicit unpack valuesspec columns =
  Q.simpleQueryArr (V.valuesU unpack valuesspec columns)
