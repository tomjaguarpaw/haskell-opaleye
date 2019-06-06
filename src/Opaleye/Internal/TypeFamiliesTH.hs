{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Opaleye.Internal.TypeFamiliesTH (makeTableAdaptorInstanceMap) where

import Data.List (foldl', nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.Internal.TH (adaptorDefinition, adaptorDefinitionFields)
import Language.Haskell.TH
import Opaleye.Internal.TypeFamilies ((:<$>), (:<*>), F, IMap, RecordField, TableField)
import Opaleye.Map (Map)


type DefaultTable h o n r p a b = Default p (TableField a h o n r) (TableField b h o n r)
type DefaultRecord h o n p a b = Default p (RecordField a h o n) (RecordField b h o n)


-- Taken, slightly modified, from product-profunctors makeAdaptorAndInstance
data ConTysFields
    = ConTys   [Type]         -- ^^ The type of each constructor field
    | FieldTys [(Name, Type)] -- ^^ The fieldname and type of each constructor field
    deriving Show


fieldTypes :: ConTysFields -> [Type]
fieldTypes (ConTys ts)   = ts
fieldTypes (FieldTys xs) = map snd xs


data DataDecInfo = DataDecInfo
    { dTyName  :: Name
    , dTyVars  :: [Name]
    , dConName :: Name
    , dConTys  :: ConTysFields
    } deriving Show


dataDec :: Info -> Q DataDecInfo
dataDec (TyConI (DataD _ tyName tyVars _ constructors _))   = dataDecInfo tyName tyVars constructors
dataDec (TyConI (NewtypeD _ tyName tyVars _ constructor _)) = dataDecInfo tyName tyVars [constructor]
dataDec _                                                   = fail "Not a data or newtype declaration"


dataDecInfo :: Name -> [TyVarBndr] -> [Con] -> Q DataDecInfo
dataDecInfo tyName tyVars constructors = do
    (conName, conTys) <- conInfo =<< onlyOneConstructor constructors
    return DataDecInfo
        { dTyName  = tyName
        , dTyVars  = map varNameOfBinder tyVars
        , dConName = conName
        , dConTys  = conTys
        }
  where
    varNameOfBinder :: TyVarBndr -> Name
    varNameOfBinder (PlainTV n)    = n
    varNameOfBinder (KindedTV n _) = n

    onlyOneConstructor [con] = return con
    onlyOneConstructor _     = fail "Can only handle one constructor"

    conInfo (NormalC cn st) = return (cn, ConTys $ map snd st)
    conInfo (RecC cn vst)   = return (cn, FieldTys $ map (\(n, _, t) -> (n, t)) vst)
    conInfo _               = fail "Cannot handle constructor type"


findFieldTypeName :: ConTysFields -> Q Name
findFieldTypeName = firstInstance findFieldApp . fieldTypes
  where
    err = fail "Could not find TableField or RecordField"

    findFieldApp (AppT (ConT t) (VarT x))
        | t == ''TableField = Just x
        | t == ''RecordField = Just x
    findFieldApp (AppT f _) = findFieldApp f
    findFieldApp _ = Nothing

    firstInstance f = maybe err return . listToMaybe . mapMaybe f


appTTableField :: Name -> [Name] -> ConTysFields -> Type -> Q Type
appTTableField tyName args fieldTys t = do
    f <- varT =<< findFieldTypeName fieldTys
    appTAll (ConT tyName) . replaceFirst f t <$> mapM varT args
  where
    replaceFirst _ _ [] = []
    replaceFirst x y (z:zs)
        | z == x    = y:zs
        | otherwise = z:replaceFirst x y zs


appTAll :: Type -> [Type] -> Type
appTAll = foldl' AppT


appEAll :: ExpQ -> [ExpQ] -> ExpQ
appEAll = foldl' appE


-- | Make adaptors and type families instances for TypeFamilies style Opaleye tables types.
makeTableAdaptorInstanceMap :: String -> Name -> Q [Dec]
makeTableAdaptorInstanceMap adaptorName consName =
    reify consName >>= dataDec >>= tableAdaptorInstanceMap (mkName adaptorName)


tableAdaptorInstanceMap :: Name -> DataDecInfo -> Q [Dec]
tableAdaptorInstanceMap adaptorName info = sequence
    [ adaptorSig adaptorName info
    , adaptorDef adaptorName info
    , instanceDef adaptorName info
    , mapTypeInstance info
    ]


p :: TypeQ
p = varT $ mkName "p"


a :: TypeQ
a = varT $ mkName "a"


b :: TypeQ
b = varT $ mkName "b"


adaptorSig :: Name -> DataDecInfo -> Q Dec
adaptorSig adaptorName DataDecInfo{dTyName, dTyVars, dConTys} = SigD adaptorName <$> adaptorType
  where
    app = appTTableField dTyName dTyVars dConTys

    adaptorType = [t| ProductProfunctor $p => $before -> $after |]
    before = app =<< [t| $p :<$> $a :<*> $b |]
    after = [t| $p $(app =<< a) $(app =<< b) |]


adaptorDef :: Name -> DataDecInfo -> Q Dec
adaptorDef adaptorName DataDecInfo{dConName, dTyVars, dConTys} = return $ fieldTypeAdaptorDef dConTys adaptorName
  where
    numTyVars = length dTyVars
    fieldTypeAdaptorDef (ConTys _)          = adaptorDefinition numTyVars dConName
    fieldTypeAdaptorDef (FieldTys fieldTys) = adaptorDefinitionFields dConName fieldTys


instanceDef :: Name -> DataDecInfo -> Q Dec
instanceDef adaptorName DataDecInfo{dTyName, dTyVars, dConName, dConTys} = instanceD ctxt instType [defDef]
  where
    app = appTTableField dTyName dTyVars dConTys

    ctxt = (:) <$> [t| ProductProfunctor $p |] <*> defaultTableCxt
    defaultTableCxt = nub <$> mapM defaultTableConstraint (fieldTypes dConTys)

    instType = [t| Default $p $(app =<< a) $(app =<< b) |]

    defDef = funD 'def [clause [] defBody []]

    numConVars = length $ fieldTypes dConTys
    defs = replicate numConVars (varE 'def)
    defBody = normalB [| $(varE adaptorName) $(appEAll (conE dConName) defs) |]


defaultTableConstraint :: Type -> Q Type
defaultTableConstraint (AppT (AppT (AppT (AppT (AppT (ConT tf) _) h) o) n) r)
    | tf == ''TableField = [t| DefaultTable $(pure h) $(pure o) $(pure n) $(pure r) $p $a $b |]
defaultTableConstraint (AppT (AppT (AppT (AppT (ConT rf) _) h) o) n)
    | rf == ''RecordField = [t| DefaultRecord $(pure h) $(pure o) $(pure n) $p $a $b |]
defaultTableConstraint _ = fail "Non TableField or RecordField Field"


mapTypeInstance :: DataDecInfo -> Q Dec
mapTypeInstance DataDecInfo{dTyName, dTyVars, dConTys} = tySynInstD ''Map (tySynEqn [g, app =<< ff] (app =<< fi))
  where
    app = appTTableField dTyName dTyVars dConTys

    f = varT $ mkName "f"
    g = varT $ mkName "g"

    ff = [t| F $f |]
    fi = [t| F (IMap $g $f) |]
