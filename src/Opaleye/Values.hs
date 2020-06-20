{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Values where

import qualified Opaleye.Internal.QueryArr as Q
import           Opaleye.Internal.Values as V
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Select              as S

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
--
-- (Please note that 'values' of an empty list generates incorrect
-- queries when mixed with @OUTER@\/@LEFT@\/@RIGHT JOIN@.  You should
-- use 'valuesSafe' instead.  'valuesSafe' will replace 'values' in
-- version 0.7.)
values :: (Default V.Valuesspec fields fields,
           Default U.Unpackspec fields fields) =>
          [fields] -> S.Select fields
values = valuesExplicit def def

valuesExplicit :: U.Unpackspec fields fields'
               -> V.Valuesspec fields fields'
               -> [fields] -> S.Select fields'
valuesExplicit unpack valuesspec fields =
  Q.productQueryArr (V.valuesU unpack valuesspec fields)

valuesSafe :: (Default V.ValuesspecSafe fields fields,
                Default U.Unpackspec fields fields) =>
               [fields] -> S.Select fields
valuesSafe = valuesSafeExplicit def def

valuesSafeExplicit :: U.Unpackspec fields fields'
                    -> V.ValuesspecSafe fields fields'
                    -> [fields] -> S.Select fields'
valuesSafeExplicit unpack valuesspec fields =
  Q.productQueryArr (V.valuesUSafe unpack valuesspec fields)

valuesUnsafe :: (Default V.Valuesspec fields fields,
                 Default U.Unpackspec fields fields) =>
                [fields] -> S.Select fields
valuesUnsafe = values

valuesUnsafeExplicit :: U.Unpackspec fields fields'
                     -> V.Valuesspec fields fields'
                     -> [fields] -> S.Select fields'
valuesUnsafeExplicit = valuesExplicit
