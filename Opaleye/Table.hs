{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Table
       ( module Opaleye.Table
       , T.TableColumn) where

import qualified Opaleye.QueryArr as QA
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Table as T
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Database.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

data Table tablecols = Table String tablecols

makeTable :: D.Default TM.TableColumnMaker strings tablecolumns =>
             Table strings -> Table tablecolumns
makeTable = makeTableExplicit D.def

queryTable :: D.Default TM.ColumnMaker tablecolumns columns =>
              Table tablecolumns -> QA.Query columns
queryTable = queryTableExplicit D.def

makeTableExplicit :: TM.TableColumnMaker strings tablecolumns ->
                     Table strings -> Table tablecolumns
makeTableExplicit t (Table n strings) =
  Table n (TM.runTableColumnMaker t strings)

queryTableExplicit :: TM.ColumnMaker tablecolumns columns ->
                      Table tablecolumns -> QA.Query columns
queryTableExplicit cm table = QA.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = queryTable' cm table (Tag.tagWith t0)

queryTable' :: TM.ColumnMaker tablecolumns columns
            -> Table tablecolumns
            -> (String -> String)
            -> (columns, PQ.PrimQuery)
queryTable' cm table tag = (primExprs, primQ) where
  (Table tableName tableCols) = table
  (projcols, primExprs) = runColumnMaker cm tag tableCols
  primQ :: PQ.PrimQuery
  primQ = PQ.BaseTable tableName projcols

runColumnMaker :: TM.ColumnMaker tablecolumns columns
                  -> (HPQ.Attribute -> String)
                  -> tablecolumns
                  -> ([(String, String)], columns)
runColumnMaker cm tag tableCols = TM.runColumnMaker cm f tableCols where
  -- Using or abusing the instance
  -- Monoid a => Applicative (a, b)
  f s = ([(tag s, s)], tag s)
