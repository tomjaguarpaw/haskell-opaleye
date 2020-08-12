{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Values(
  valuesSafe,
  -- * Explicit versions
  valuesSafeExplicit,
  -- * Adaptors
  V.ValuesspecSafe,
  V.valuesspecField,
  -- * Deprecated versions
  values,
  valuesUnsafe,
  valuesExplicit,
  valuesUnsafeExplicit,
  ) where

import qualified Opaleye.Internal.QueryArr as Q
import           Opaleye.Internal.Values as V
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Select              as S

import           Data.Profunctor.Product.Default (Default, def)

-- | Please note that 'values' of an empty list generates incorrect
-- queries when mixed with @OUTER@\/@LEFT@\/@RIGHT JOIN@.  You should
-- use 'valuesSafe' instead.  'valuesSafe' will replace 'values' in
-- version 0.7.
values :: (Default V.Valuesspec fields fields,
           Default U.Unpackspec fields fields) =>
          [fields] -> S.Select fields
values = valuesExplicit def def

-- | Please note that 'valuesExplict' of an empty list generates
-- incorrect queries when mixed with @OUTER@\/@LEFT@\/@RIGHT JOIN@.
-- You should use 'valuesSafeExplicit' instead.  'valuesSafeExplicit'
-- will replace 'valuesExplicit' in version 0.7.
valuesExplicit :: U.Unpackspec fields fields'
               -> V.Valuesspec fields fields'
               -> [fields] -> S.Select fields'
valuesExplicit unpack valuesspec fields =
  Q.productQueryArr (V.valuesU unpack valuesspec fields)

-- | 'valuesSafe' implements Postgres's @VALUES@ construct and allows you
-- to create a @SELECT@ that consists of the given rows.
--
-- Example type specialization:
--
-- @
-- valuesSafe :: [(Field a, Field b)] -> Select (Field a, Field b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- valuesSafe :: [Foo (Field a) (Field b) (Field c)] -> S.Select (Foo (Field a) (Field b) (Field c))
-- @
valuesSafe :: Default V.ValuesspecSafe fields fields =>
               [fields] -> S.Select fields
valuesSafe = valuesSafeExplicit def

valuesSafeExplicit :: V.ValuesspecSafe fields fields'
                   -> [fields] -> S.Select fields'
valuesSafeExplicit valuesspec fields =
  Q.productQueryArr (V.valuesUSafe valuesspec fields)

-- | Forward-compatible version of unsafe 'values' that will not be
-- deprecated in 0.7, but in 0.8.
valuesUnsafe :: (Default V.ValuesspecUnsafe fields fields,
                 Default U.Unpackspec fields fields) =>
                [fields] -> S.Select fields
valuesUnsafe = values

-- | Forward compatible version of unsafe 'valuesExplicit' that will
-- not be deprecated in 0.7, but in 0.8.
valuesUnsafeExplicit :: U.Unpackspec fields fields'
                     -> V.ValuesspecUnsafe fields fields'
                     -> [fields] -> S.Select fields'
valuesUnsafeExplicit = valuesExplicit
