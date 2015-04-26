{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Table (module Opaleye.Table,
                      View,
                      Writer,
                      Table(Table),
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
queryTable :: D.Default TM.ColumnMaker columns columns =>
              Table a columns -> Q.Query columns
queryTable = queryTableExplicit D.def

queryTableExplicit :: TM.ColumnMaker tablecolumns columns ->
                     Table a tablecolumns -> Q.Query columns
queryTableExplicit cm table = Q.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = T.queryTable cm table t0

required :: String -> TableProperties (Column a) (Column a)
required columnName = T.TableProperties
  (T.required columnName)
  (View (Column (HPQ.BaseTableAttrExpr columnName)))

optional :: String -> TableProperties (Maybe (Column a)) (Column a)
optional columnName = T.TableProperties
  (T.optional columnName)
  (View (Column (HPQ.BaseTableAttrExpr columnName)))
