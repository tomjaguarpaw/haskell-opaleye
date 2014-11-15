{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.TableMaker where

import qualified Opaleye.Column as C
import qualified Opaleye.Internal.PackMap as PM

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Database.HaskellDB.PrimQuery as PQ


-- TODO: This should be the equivalent of a Control.Lens.Setter
newtype TableColumnMaker strings columns =
  TableColumnMaker (PM.PackMap () () strings columns)

newtype ColumnMaker columns columns' =
  ColumnMaker (PM.PackMap PQ.PrimExpr PQ.PrimExpr columns columns')

runTableColumnMaker :: TableColumnMaker strings tablecolumns ->
                       strings -> tablecolumns
runTableColumnMaker (TableColumnMaker f) = PM.over f id

runColumnMaker :: Applicative f
                  => ColumnMaker tablecolumns columns
                  -> (PQ.PrimExpr -> f PQ.PrimExpr)
                  -> tablecolumns -> f columns
runColumnMaker (ColumnMaker f) = PM.packmap f

-- There's surely a way of simplifying this implementation
tableColumn :: TableColumnMaker String (C.Column a)
tableColumn = TableColumnMaker
              (PM.PackMap (\f s -> fmap (const ((C.Column . PQ.AttrExpr) s)) (f ())))

column :: ColumnMaker (C.Column a) (C.Column a)
column = ColumnMaker
         (PM.PackMap (\f (C.Column s)
                      -> fmap C.Column (f s)))

instance Default TableColumnMaker String (C.Column a) where
  def = tableColumn

instance Default ColumnMaker (C.Column a) (C.Column a) where
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
