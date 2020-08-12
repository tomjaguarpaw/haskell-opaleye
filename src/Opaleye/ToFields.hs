module Opaleye.ToFields (C.toFields, C.ToFields, module Opaleye.ToFields) where

import qualified Opaleye.Constant as C

toFieldsExplicit :: C.ToFields haskells fields -> haskells -> fields
toFieldsExplicit = C.constantExplicit

toToFields :: (haskells -> fields) -> C.ToFields haskells fields
toToFields = C.Constant
