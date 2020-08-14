{-# LANGUAGE FlexibleContexts #-}

module Opaleye.ToFields (C.toFields, toFieldsI, C.ToFields, module Opaleye.ToFields) where

import qualified Opaleye.Constant as C
import           Opaleye.Internal.Inferrable (Inferrable, runInferrable)

import qualified Data.Profunctor.Product.Default as D

toFieldsExplicit :: C.ToFields haskells fields -> haskells -> fields
toFieldsExplicit = C.constantExplicit

toToFields :: (haskells -> fields) -> C.ToFields haskells fields
toToFields = C.Constant

-- | Version of 'C.toFields' with better type inference
toFieldsI :: (D.Default (Inferrable C.ToFields) haskells fields)
          => haskells
          -- ^
          -> fields
toFieldsI = toFieldsExplicit (runInferrable D.def)
