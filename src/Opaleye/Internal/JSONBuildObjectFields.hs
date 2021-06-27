module Opaleye.Internal.JSONBuildObjectFields
  ( JSONBuildObjectFields,
    jsonBuildObjectField,
    jsonBuildObject,
  )
where

import Opaleye.Internal.Column (Column (Column))
import Opaleye.Internal.HaskellDB.PrimQuery (Literal (StringLit), PrimExpr (ConstExpr, FunExpr))
import Opaleye.Internal.PGTypesExternal (SqlJson)
import Data.Semigroup

-- | Combine @JSONBuildObjectFields@ using @('<>')@
newtype JSONBuildObjectFields
  = JSONBuildObjectFields [(String, PrimExpr)]

instance Semigroup JSONBuildObjectFields where
  (<>)
    (JSONBuildObjectFields a)
    (JSONBuildObjectFields b) =
      JSONBuildObjectFields $ a <> b

instance Monoid JSONBuildObjectFields where
  mempty = JSONBuildObjectFields mempty
  mappend = (<>)

-- | Given a label and a column, generates a pair for use with @jsonBuildObject@
jsonBuildObjectField :: String
                     -- ^ Field name
                     -> Column a
                     -- ^ Field value
                     -> JSONBuildObjectFields
jsonBuildObjectField f (Column v) = JSONBuildObjectFields [(f, v)]

-- | Create an 'SqlJson' object from a collection of fields.
--
--   Note: This is implemented as a variadic function in postgres, and as such, is limited to 50 arguments, or 25 key-value pairs.
jsonBuildObject :: JSONBuildObjectFields -> Column SqlJson
jsonBuildObject (JSONBuildObjectFields jbofs) = Column $ FunExpr "json_build_object" args
  where
    args = concatMap mapLabelsToPrimExpr jbofs
    mapLabelsToPrimExpr (label, expr) = [ConstExpr $ StringLit label, expr]
