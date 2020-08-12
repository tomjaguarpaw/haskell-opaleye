{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Distinct (distinct,
                         Distinctspec,
                         -- * Explicit versions
                         distinctExplicit,
                         -- * Adaptors
                         distinctspecField,
                         distinctspecMaybeFields,
                        )
       where

import           Opaleye.Select (Select)
import           Opaleye.Internal.Distinct

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
-- If you want to run 'distinct' on 'Select.SelectArr's you should
-- apply 'Opaleye.Lateral.laterally' to it:
--
-- @
-- 'Opaleye.Lateral.laterally' 'distinct' :: 'Data.Profunctor.Product.Default' 'Distinctspec' fields fields => 'Opaleye.Select.SelectArr' i fields -> 'Opaleye.Select.SelectArr' i fields
-- @
distinct :: D.Default Distinctspec fields fields =>
            Select fields -> Select fields
distinct = distinctExplicit D.def
