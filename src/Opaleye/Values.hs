{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Values(
  values,
  -- * Explicit versions
  valuesExplicit,
  -- * Adaptors
  V.Valuesspec,
  V.valuesspecField,
  ) where

import qualified Opaleye.Internal.Values as V
import qualified Opaleye.Select              as S

import qualified Data.List.NonEmpty as NEL
import           Data.Profunctor.Product.Default (Default, def)

-- | 'values' implements Postgres's @VALUES@ construct and allows you
-- to create a @SELECT@ that consists of the given rows.
--
-- Example type specialization:
--
-- @
-- values :: [(Field a, Field b)] -> Select (Field a, Field b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- values :: [Foo (Field a) (Field b) (Field c)] -> S.Select (Foo (Field a) (Field b) (Field c))
-- @
values :: Default V.Valuesspec fields fields
       => [fields] -> S.Select fields
values = valuesExplicit def

valuesExplicit :: V.Valuesspec fields fields'
               -> [fields] -> S.Select fields'
valuesExplicit (V.ValuesspecSafe nullspec rowspec) fields = case NEL.nonEmpty fields of
  Nothing -> V.emptySelectExplicit nullspec
  Just rows -> V.nonEmptyValues rowspec rows
