{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Table (module Opaleye.Table,
                      View(..),
                      Writer(..),
                      Writeable(..)) where

import           Opaleye.Column (Column(Column))
import qualified Opaleye.QueryArr as QA
import qualified Opaleye.Internal.Table as T
import           Opaleye.Internal.Table (View(View), Writer(Writer),
                                         Writeable(Writeable))
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.PackMap as PM

import qualified Data.Profunctor.Product.Default as D
import           Control.Applicative (Applicative, pure)

makeView :: D.Default TM.ViewColumnMaker strings tablecolumns =>
             View strings -> View tablecolumns
makeView = makeViewExplicit D.def

queryView :: D.Default TM.ColumnMaker columns columns =>
              View columns -> QA.Query columns
queryView = queryViewExplicit D.def

makeViewExplicit :: TM.ViewColumnMaker strings tablecolumns ->
                     View strings -> View tablecolumns
makeViewExplicit t (View n strings) =
  View n (TM.runViewColumnMaker t strings)

queryViewExplicit :: TM.ColumnMaker tablecolumns columns ->
                      View tablecolumns -> QA.Query columns
queryViewExplicit cm table = QA.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = T.queryView' cm table t0

required :: String -> Writer (Column a) (Column a)
required columnName =
  Writer (PM.PackMap (\f (Column primExpr) -> f (primExpr, columnName)))

optional :: String -> Writer (Maybe (Column a)) (Column a)
optional columnName =
  Writer (PM.PackMap (\f c -> case c of
                         Nothing -> pure ()
                         Just (Column primExpr) -> f (primExpr, columnName)))
