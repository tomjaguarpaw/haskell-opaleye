{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.Schema where

import           Opaleye.Internal.Column
import           Opaleye.Internal.Table as IT
import           Opaleye.Internal.TableMaker as TM
import           Opaleye.Internal.PackMap as PM
import           Opaleye.PGTypes

import           Data.Profunctor (Profunctor, dimap, lmap)
import           Data.Profunctor.Product as PP

import Data.Monoid

import qualified Data.Profunctor.Product.Default as D

class PGType a where
  data SchemaOptions a
  pgColumnDefinition :: SchemaOptions a -> String
  defaultOptions :: SchemaOptions a

instance PGType PGInt8 where
  data SchemaOptions PGInt8 = NoIntOptions
  pgColumnDefinition _ = "SERIAL"
  defaultOptions = NoIntOptions
  
instance PGType PGText where
  data SchemaOptions PGText = Length Int | Unspecified
  pgColumnDefinition (Length x) = "varchar (" <> show x <> ")"
  pgColumnDefinition _ = "text"
  defaultOptions = Unspecified

data TableSchema = TableSchema String [UntypedColumn]

newtype UntypedColumn = UntypedColumn { unUntypedColumn :: forall a. TM.TableColumn a }

discardSchema :: IT.Table a b -> (String, IT.TableProperties a b)
discardSchema (IT.TableWithSchema _ s p) = (s, p)
discardSchema (IT.Table s p) = (s, p)

tableSchema :: forall read write.
  (D.Default SchemaMaker read write) =>
  IT.Table write read -> TableSchema
tableSchema (discardSchema -> (tableName, (IT.TableProperties _ (View tableColumns)))) =
  TableSchema tableName columns
  where
  s :: SchemaMaker read write
  s = D.def
  SchemaMaker (PM.PackMap pm) = s
  extractor d = ([d], ())
  (columns, ()) = pm extractor tableColumns

data ForeignKey = ForeignKey [String] [String]

foreignKey ::
  forall from from' to to' fk.
  (D.Default SchemaMaker fk fk) =>
  IT.Table from' from -> (from -> fk) -> IT.Table to' to -> (to -> fk) -> ForeignKey
foreignKey tableFrom selectSubsetFrom tableTo selectSubsetTo = let
  extractor (unUntypedColumn -> TM.TableColumn name' _) = ([name'], ())
  (snd . discardSchema -> (IT.TableProperties _ (View tableColsFrom))) = tableFrom
  (snd . discardSchema -> (IT.TableProperties _ (View tableColsTo))) = tableTo
  keyFrom = selectSubsetFrom tableColsFrom
  keyTo = selectSubsetTo tableColsTo
  s1 :: SchemaMaker fk fk
  s1 = D.def
  (SchemaMaker (PM.PackMap pm)) = s1
  (columnsFrom, ()) = pm extractor keyFrom
  (columnsTo, ()) = pm extractor keyTo
  in ForeignKey columnsFrom columnsTo

columnSchemaMaker :: SchemaMaker (TM.TableColumn any) b
columnSchemaMaker = SchemaMaker (PM.PackMap (\f (TM.TableColumn x y) -> f (UntypedColumn (TM.TableColumn x y))))

instance D.Default SchemaMaker (TM.TableColumn a) (Column a) where
  def = columnSchemaMaker

instance D.Default SchemaMaker (TM.TableColumn a) (TM.TableColumn a) where
  def = columnSchemaMaker

instance D.Default SchemaMaker (TM.TableColumn a) (Maybe (Column a)) where
  def = columnSchemaMaker

newtype SchemaMaker read dummy =
  SchemaMaker (PM.PackMap UntypedColumn () read ())

instance Functor (SchemaMaker a) where
  fmap _ (SchemaMaker g) = SchemaMaker (g)

instance Applicative (SchemaMaker a) where
  pure x = SchemaMaker (fmap (const ()) (pure x))
  SchemaMaker fx <*> SchemaMaker x = SchemaMaker $
    pure (const id) <*> fx <*> x

instance Profunctor SchemaMaker where
  dimap f _ (SchemaMaker q) = SchemaMaker (lmap f q)

instance ProductProfunctor SchemaMaker where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct
