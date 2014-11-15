{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Table (module Opaleye.Table,
                      Table(..),
                      Writer(..),
                      Writeable(..)) where

import           Opaleye.Column (Column(Column))
import qualified Opaleye.QueryArr as QA
import qualified Opaleye.Internal.Table as T
import           Opaleye.Internal.Table (Table(Table), Writer(Writer),
                                         Writeable(Writeable))
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.PackMap as PM

import qualified Data.Profunctor.Product.Default as D
import           Control.Applicative (Applicative, pure)

makeTable :: D.Default TM.TableColumnMaker strings tablecolumns =>
             Table strings -> Table tablecolumns
makeTable = makeTableExplicit D.def

queryTable :: D.Default TM.ColumnMaker columns columns =>
              Table columns -> QA.Query columns
queryTable = queryTableExplicit D.def

makeTableExplicit :: TM.TableColumnMaker strings tablecolumns ->
                     Table strings -> Table tablecolumns
makeTableExplicit t (Table n strings) =
  Table n (TM.runTableColumnMaker t strings)

queryTableExplicit :: TM.ColumnMaker tablecolumns columns ->
                      Table tablecolumns -> QA.Query columns
queryTableExplicit cm table = QA.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = T.queryTable' cm table t0

required :: String -> Writer (Column a) (Column a)
required columnName =
  Writer (PM.PackMap (\f (Column primExpr) -> f (primExpr, columnName)))

optional :: String -> Writer (Maybe (Column a)) (Column a)
optional columnName =
  Writer (PM.PackMap (\f c -> case c of
                         Nothing -> pure ()
                         Just (Column primExpr) -> f (primExpr, columnName)))
