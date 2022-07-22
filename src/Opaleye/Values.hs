{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Values(
  values,
  -- * Explicit versions
  valuesExplicit,
  -- * Adaptors
  V.Valuesspec,
  V.valuesspecField,
  -- * Deprecated versions
  valuesSafe,
  valuesSafeExplicit,
  valuesUnsafe,
  valuesUnsafeExplicit,
  V.ValuesspecSafe,
  ) where

import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Values as V
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Select              as S

import qualified Data.List.NonEmpty as NEL
import           Data.Profunctor.Product.Default (Default, def)

{-# DEPRECATED valuesUnsafe "Use 'values' instead.  Will be removed in 0.10." #-}
valuesUnsafe :: (Default V.ValuesspecUnsafe fields fields,
                 Default U.Unpackspec fields fields) =>
                [fields] -> S.Select fields
valuesUnsafe = valuesUnsafeExplicit def def

{-# DEPRECATED valuesUnsafeExplicit "Use 'values' instead.  Will be removed in 0.10." #-}
valuesUnsafeExplicit :: U.Unpackspec fields fields'
                     -> V.ValuesspecUnsafe fields fields'
                     -> [fields] -> S.Select fields'
valuesUnsafeExplicit unpack valuesspec fields =
  Q.productQueryArr $ do
  t <- Tag.fresh
  pure (V.valuesU unpack valuesspec fields ((), t))

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
valuesExplicit valuesspec@(V.ValuesspecSafe nullspec _) fields = case NEL.nonEmpty fields of
  Nothing -> V.emptySelectExplicit nullspec
  Just rows' -> V.nonEmptyValues valuesspec rows'

{-# DEPRECATED valuesSafe "Use 'values' instead.  Will be removed in 0.10." #-}
valuesSafe :: Default V.Valuesspec fields fields
           => [fields] -> S.Select fields
valuesSafe = values

{-# DEPRECATED valuesSafeExplicit "Use 'values' instead.  Will be removed in 0.10." #-}
valuesSafeExplicit :: V.Valuesspec fields fields'
                   -> [fields] -> S.Select fields'
valuesSafeExplicit = valuesExplicit
