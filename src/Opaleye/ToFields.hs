{-# LANGUAGE FlexibleContexts #-}

module Opaleye.ToFields (-- * Creating 'Field's from Haskell values
                         toFields,
                         toFieldsI,
                         C.toField,
                         toFieldI,
                         -- * Creating @ToFields@
                         C.toToFields,
                         -- * Explicit versions
                         toFieldsExplicit,
                         -- * Adaptor
                         C.ToFields,
                         ) where

import qualified Opaleye.Internal.Constant as C
import           Opaleye.Internal.Inferrable (Inferrable, runInferrable)
import qualified Opaleye.Field as F

import qualified Data.Profunctor.Product.Default as D

toFieldsExplicit :: C.ToFields haskells fields -> haskells -> fields
toFieldsExplicit = C.constantExplicit

-- | 'toFields' provides a convenient typeclass wrapper around the
-- 'Opaleye.Field.Field_' creation functions in "Opaleye.SqlTypes".  Besides
-- convenience it doesn't provide any additional functionality.
--
-- It can be used with functions like 'Opaleye.Manipulation.runInsert'
-- to insert custom Haskell types into the database.
-- The following is an example of a function for inserting custom types.
--
-- @
--   customInsert
--      :: ( 'D.Default' 'ToFields' haskells fields )
--      => Connection
--      -> 'Opaleye.Table' fields fields'
--      -> [haskells]
--      -> IO Int64
--   customInsert conn table haskells = 'Opaleye.Manipulation.runInsert_' conn 'Opaleye.Manipulation.Insert' {
--         iTable      = table
--       , iRows       = map 'toFields' haskells
--       , iReturning  = rCount
--       , iOnConflict = Nothing
--     }
-- @
--
-- In order to use this function with your custom types, you need to define an
-- instance of 'D.Default' 'ToFields' for your custom types.
toFields :: D.Default C.ToFields haskells fields => haskells -> fields
toFields = C.toFields

-- | Version of 'C.toFields' with better type inference
toFieldsI :: (D.Default (Inferrable C.ToFields) haskells fields)
          => haskells
          -- ^
          -> fields
toFieldsI = toFieldsExplicit (runInferrable D.def)

-- | Version of 'C.toField' with better type inference
toFieldI :: (D.Default (Inferrable C.ToFields) haskells (F.Field field))
         => haskells
         -- ^
         -> F.Field field
toFieldI = toFieldsExplicit (runInferrable D.def)
