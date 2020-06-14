{-# LANGUAGE FlexibleContexts #-}

module Opaleye.ToFields (toFieldsI, C.ToFields, module Opaleye.ToFields) where

import qualified Opaleye.Constant as C
import           Opaleye.Internal.Inferrable (Inferrable, runInferrable)

import qualified Data.Profunctor.Product.Default as D

toFieldsExplicit :: C.ToFields haskells fields -> haskells -> fields
toFieldsExplicit = C.constantExplicit

toFields :: D.Default C.ToFields haskells fields => haskells -> fields
toFields = C.toFields

toToFields :: (haskells -> fields) -> C.ToFields haskells fields
toToFields = C.ToFields

-- | Version of 'C.toFields' with better type inference
toFieldsI :: (D.Default (Inferrable C.ToFields) haskells fields)
          => haskells
          -- ^
          -> fields
toFieldsI = toFieldsExplicit (runInferrable D.def)
