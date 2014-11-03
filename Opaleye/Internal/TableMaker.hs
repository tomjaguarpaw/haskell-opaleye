{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.TableMaker where

import qualified Opaleye.Internal.Table as T
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.PackMap as PM

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Database.HaskellDB.PrimQuery as PQ


newtype TableColumnMaker strings tablecolumns =
  TableColumnMaker (PM.PackMap () () strings tablecolumns)

newtype ColumnMaker tablecolumns columns =
  ColumnMaker (PM.PackMap String String tablecolumns columns)

runTableColumnMaker :: TableColumnMaker strings tablecolumns ->
                       strings -> tablecolumns
runTableColumnMaker (TableColumnMaker f) = PM.over f id

runColumnMaker :: Applicative f
                  => ColumnMaker tablecolumns columns
                  -> (String -> f (String))
                  -> tablecolumns -> f columns
runColumnMaker (ColumnMaker f) = PM.packmap f

-- There's surely a way of simplifying this implementation
tableColumn :: TableColumnMaker String (T.TableColumn a)
tableColumn = TableColumnMaker
              (PM.PackMap (\f s -> fmap (const (T.TableColumn s)) (f ())))

column :: ColumnMaker (T.TableColumn a) (C.Column a)
column = ColumnMaker
         (PM.PackMap (\f (T.TableColumn s)
                      -> fmap (C.Column . PQ.AttrExpr) (f s)))

instance Default TableColumnMaker String (T.TableColumn a) where
  def = tableColumn

instance Default ColumnMaker (T.TableColumn a) (C.Column a) where
  def = column

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (TableColumnMaker a) where
  fmap f (TableColumnMaker g) = TableColumnMaker (fmap f g)

instance Applicative (TableColumnMaker a) where
  pure = TableColumnMaker . pure
  TableColumnMaker f <*> TableColumnMaker x = TableColumnMaker (f <*> x)

instance Profunctor TableColumnMaker where
  dimap f g (TableColumnMaker q) = TableColumnMaker (dimap f g q)

instance ProductProfunctor TableColumnMaker where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance Functor (ColumnMaker a) where
  fmap f (ColumnMaker g) = ColumnMaker (fmap f g)

instance Applicative (ColumnMaker a) where
  pure = ColumnMaker . pure
  ColumnMaker f <*> ColumnMaker x = ColumnMaker (f <*> x)

instance Profunctor ColumnMaker where
  dimap f g (ColumnMaker q) = ColumnMaker (dimap f g q)

instance ProductProfunctor ColumnMaker where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

--}
