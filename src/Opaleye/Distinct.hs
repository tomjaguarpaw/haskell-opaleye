module Opaleye.Distinct (distinct,
                         distinctOn,
                         distinctOnBy,
                         -- * Explicit versions
                         distinctExplicit,
                         -- * Adaptors
                         Distinctspec,
                         distinctspecField,
                         distinctspecMaybeFields,
                         -- * Deprecated
                         distinctOnCorrect,
                         distinctOnByCorrect,
                        )
       where

import           Opaleye.Select (Select)
import           Opaleye.Internal.Distinct
import           Opaleye.Order

import qualified Data.Profunctor.Product.Default as D
import Opaleye.Internal.Unpackspec (Unpackspec)

-- | Remove duplicate rows from the 'Select'.
--
-- Example type specialization:
--
-- @
-- distinct :: Select (Field a, Field b) -> Select (Field a, Field b)
-- @
--
-- Assuming the @makeAdaptorAndInstanceInferrable@ splice has been run for the product type @Foo@:
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
            D.Default Unpackspec fields fields =>
            Select fields -> Select fields
distinct = distinctExplicit D.def D.def
