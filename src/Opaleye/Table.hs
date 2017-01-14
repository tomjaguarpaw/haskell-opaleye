{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.Table (module Opaleye.Table,
                      -- * Other
                      View,
                      Writer,
                      TM.TableColumn,
                      Table(Table, TableWithSchema),
                      TableProperties) where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Table as T
import           Opaleye.Internal.Table (View(View), Table, Writer,
                                         TableProperties)
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag

import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Opaleye.Internal.Schema as S

type family ColumnFromTableColumn a
type instance ColumnFromTableColumn (TM.TableColumn a) = (Column a)
type instance ColumnFromTableColumn (a, b) = (ColumnFromTableColumn a, ColumnFromTableColumn b)

-- | Example type specialization:
--
-- @
-- queryTable :: Table w (Column a, Column b) -> Query (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- queryTable :: Table w (Foo (Column a) (Column b) (Column c)) -> Query (Foo (Column a) (Column b) (Column c))
-- @
queryTable :: (D.Default TM.ColumnMaker columns columns, D.Default TM.TableProjector tableColumns columns, ColumnFromTableColumn tableColumns ~ columns) =>
              Table a tableColumns -> Q.Query columns
queryTable = queryTableExplicit D.def D.def

required' :: S.PGType a => S.SchemaOptions a -> String -> TableProperties (Column a) (TM.TableColumn a)
required' schemaOptions columnName = T.TableProperties
  (T.required columnName)
  (View (TM.TableColumn columnName (S.pgColumnDefinition schemaOptions) ))

-- | 'required' is for columns which are not 'optional'.  You must
-- provide them on writes.
required :: S.PGType a => String -> TableProperties (Column a) (TM.TableColumn a)
required = required' S.defaultOptions

-- | 'optional' is for columns that you can omit on writes, such as
--  columns which have defaults or which are SERIAL.
optional :: S.PGType a => String -> TableProperties (Maybe (Column a)) (TM.TableColumn a)
optional columnName = T.TableProperties
  (T.optional columnName)
  (View (TM.TableColumn columnName ""))
  
-- * Explicit versions

queryTableExplicit :: TM.ColumnMaker columns columns ->
                      TM.TableProjector tablecolumns columns ->
                     Table a tablecolumns -> Q.Query columns
queryTableExplicit cm tp table = Q.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = T.queryTable cm tp table t0
