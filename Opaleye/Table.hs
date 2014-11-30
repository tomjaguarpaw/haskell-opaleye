{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Table (module Opaleye.Table,
                      View,
                      Writer,
                      Table(Table),
                      TableProperties) where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Table as T
import           Opaleye.Internal.Table (View(View), Writer(Writer),
                                         Table, TableProperties)
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.PackMap as PM

import qualified Data.Profunctor.Product.Default as D
import           Control.Applicative (Applicative, pure)

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

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
  (Writer (PM.PackMap (\f (Column primExpr) -> f (primExpr, columnName))))
  (View (Column (HPQ.AttrExpr columnName)))

optional :: String -> TableProperties (Maybe (Column a)) (Column a)
optional columnName = T.TableProperties
  (Writer (PM.PackMap (\f c -> case c of
                          Nothing -> pure ()
                          Just (Column primExpr) -> f (primExpr, columnName))))
  (View (Column (HPQ.AttrExpr columnName)))
