{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Values(
  values,
  -- * Explicit versions
  valuesExplicit,
  -- * Adaptors
  V.Valuesspec,
  V.ValuesspecSafe,
  V.valuesspecField,
  -- * Deprecated versions
  valuesSafe,
  valuesSafeExplicit,
  valuesUnsafe,
  valuesUnsafeExplicit,
  ) where

import qualified Opaleye.Internal.QueryArr as Q
import           Opaleye.Internal.Values as V
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Select              as S

import           Data.Profunctor.Product.Default (Default, def)

-- | Do not use.  Will be deprecated in 0.8.
valuesUnsafe :: (Default V.ValuesspecUnsafe fields fields,
                 Default U.Unpackspec fields fields) =>
                [fields] -> S.Select fields
valuesUnsafe = valuesUnsafeExplicit def def

-- | Do not use.  Will be deprecated in 0.8.
valuesUnsafeExplicit :: U.Unpackspec fields fields'
                     -> V.ValuesspecUnsafe fields fields'
                     -> [fields] -> S.Select fields'
valuesUnsafeExplicit unpack valuesspec fields =
  Q.productQueryArr (V.valuesU unpack valuesspec fields)

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
valuesExplicit valuesspec fields =
  Q.productQueryArr (V.valuesUSafe valuesspec fields)

-- | Use 'values' instead.  Will be deprecated in 0.8.
valuesSafe :: Default V.Valuesspec fields fields
           => [fields] -> S.Select fields
valuesSafe = values

-- | Use 'valuesExplicit' instead.  Will be deprecated in 0.8.
valuesSafeExplicit :: V.Valuesspec fields fields'
                   -> [fields] -> S.Select fields'
valuesSafeExplicit = valuesExplicit
