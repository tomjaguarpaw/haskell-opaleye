{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Internal.Table where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Data.Profunctor (Profunctor, dimap, lmap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Control.Applicative (Applicative, pure, (<*>), liftA2)

data Table writerColumns viewColumns =
  Table String (TableProperties writerColumns viewColumns)

data TableProperties writerColumns viewColumns =
  TableProperties (Writer writerColumns viewColumns) (View viewColumns)

data View columns = View columns

-- If we switch to a more lens-like approach to PackMap this should be
-- the equivalent of a Fold

-- There's no reason the second parameter should exist except that we
-- use ProductProfunctors more than ProductContravariants so it makes
-- things easier if we make it one of the former.
data Writer columns dummy =
  Writer (PM.PackMap (HPQ.PrimExpr, String) () columns ())

queryTable :: TM.ColumnMaker viewColumns columns
            -> Table writerColumns viewColumns
            -> Tag.Tag
            -> (columns, PQ.PrimQuery)
queryTable cm table tag = (primExprs, primQ) where
  (Table tableName (TableProperties _ (View tableCols))) = table
  (primExprs, projcols) = runColumnMaker cm tag tableCols
  primQ :: PQ.PrimQuery
  primQ = PQ.BaseTable tableName projcols

runColumnMaker :: TM.ColumnMaker tablecolumns columns
                  -> Tag.Tag
                  -> tablecolumns
                  -> (columns, [(HPQ.Symbol, HPQ.PrimExpr)])
runColumnMaker cm tag tableCols = PM.run (TM.runColumnMaker cm f tableCols) where
  f = PM.extractAttrPE mkName tag
  -- The non-AttrExpr PrimExprs are not created by 'makeView' or a
  -- 'ViewColumnMaker' so could only arise from an fmap (if we
  -- implemented a Functor instance) or a direct manipulation of the
  -- tablecols contained in the View (which would be naughty)
  mkName pe i = (++ i) $ case pe of
    HPQ.BaseTableAttrExpr columnName -> columnName
    _ -> "tablecolumn"

runWriter :: Writer columns columns' -> columns -> [(HPQ.PrimExpr, String)]
runWriter (Writer (PM.PackMap f)) columns = outColumns
  where extractColumns t = ([t], ())
        (outColumns, ()) = f extractColumns columns

required :: String -> Writer (Column a) (Column a)
required columnName =
  Writer (PM.PackMap (\f (Column primExpr) -> f (primExpr, columnName)))

optional :: String -> Writer (Maybe (Column a)) (Column a)
optional columnName =
  Writer (PM.PackMap (\f c -> case c of
                         Nothing -> pure ()
                         Just (Column primExpr) -> f (primExpr, columnName)))

-- {

-- Boilerplate instance definitions

instance Functor (Writer a) where
  fmap _ (Writer g) = Writer g

instance Applicative (Writer a) where
  pure x = Writer (fmap (const ()) (pure x))
  Writer f <*> Writer x = Writer (liftA2 (\_ _ -> ()) f x)

instance Profunctor Writer where
  dimap f _ (Writer h) = Writer (lmap f h)

instance ProductProfunctor Writer where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance Functor (TableProperties a) where
  fmap f (TableProperties w (View v)) = TableProperties (fmap f w) (View (f v))

instance Applicative (TableProperties a) where
  pure x = TableProperties (pure x) (View x)
  TableProperties fw (View fv) <*> TableProperties xw (View xv) =
    TableProperties (fw <*> xw) (View (fv xv))

instance Profunctor TableProperties where
  dimap f g (TableProperties w (View v)) = TableProperties (dimap f g w)
                                                            (View (g v))
instance ProductProfunctor TableProperties where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance Functor (Table a) where
  fmap f (Table s tp) = Table s (fmap f tp)

-- }
