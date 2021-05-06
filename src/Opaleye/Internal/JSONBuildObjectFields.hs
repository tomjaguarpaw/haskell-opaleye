module Opaleye.Internal.JSONBuildObjectFields
  ( JSONBuildObjectFields,
    jsonBuildObjectField,
    jsonBuildObject,
    jsonCoalesce
  )
where

import Opaleye.Internal.Column (Column (Column), Nullable)
import Opaleye.Internal.HaskellDB.PrimQuery (Literal (StringLit), PrimExpr (ConstExpr, FunExpr))
import Opaleye.Internal.PGTypesExternal (SqlJson, pgJSON)
import Opaleye.Column (fromNullable)
import Data.Semigroup

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

jsonBuildObjectField :: String -> Column a -> JSONBuildObjectFields
jsonBuildObjectField f (Column v) = JSONBuildObjectFields [(f, v)]

jsonBuildObject :: JSONBuildObjectFields -> Column SqlJson
jsonBuildObject (JSONBuildObjectFields jbofs) = Column $ FunExpr "json_build_object" args
  where
    args = concatMap mapLabelsToPrimExpr jbofs
    mapLabelsToPrimExpr (label, expr) = [ConstExpr $ StringLit label, expr]

-- TODO maybe remove this
jsonCoalesce :: Column (Nullable SqlJson) -> Column SqlJson
jsonCoalesce = fromNullable $ pgJSON "[]"