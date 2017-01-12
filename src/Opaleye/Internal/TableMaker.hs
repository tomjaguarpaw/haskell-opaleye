{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.TableMaker where

import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.PackMap as PM

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Functor.Identity as I


-- If we switch to a more lens-like approach to PackMap this should be
-- the equivalent of a Setter
newtype ViewColumnMaker strings columns =
  ViewColumnMaker (PM.PackMap () () strings columns)

newtype ColumnMaker columns columns' =
  ColumnMaker (PM.PackMap HPQ.PrimExpr HPQ.PrimExpr columns columns')

data TableColumn a = TableColumn {
  name :: String ,
  pgType :: String ,
  options :: String }
  
newtype TableProjector columns columns' = TableProjector (columns -> I.Identity columns')

runViewColumnMaker :: ViewColumnMaker strings tablecolumns ->
                       strings -> tablecolumns
runViewColumnMaker (ViewColumnMaker f) = PM.overPM f id

runColumnMaker :: Applicative f
                  => ColumnMaker tablecolumns columns
                  -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                  -> tablecolumns -> f columns
runColumnMaker (ColumnMaker f) = PM.traversePM f

-- There's surely a way of simplifying this implementation
tableColumn :: ViewColumnMaker String (C.Column a)
tableColumn = ViewColumnMaker
              (PM.PackMap (\f s -> fmap (const (mkColumn s)) (f ())))
  where mkColumn = IC.Column . HPQ.BaseTableAttrExpr

column :: ColumnMaker (C.Column a) (C.Column a)
column = ColumnMaker
         (PM.PackMap (\f (IC.Column s)
                      -> fmap IC.Column (f s)))

tableProjector :: TableProjector (TableColumn a) (IC.Column a)
tableProjector = TableProjector (\(TableColumn name' _ _) -> (I.Identity . IC.Column . HPQ.BaseTableAttrExpr) name')
  
instance Default ViewColumnMaker String (C.Column a) where
  def = tableColumn

instance Default ColumnMaker (C.Column a) (C.Column a) where
  def = column

instance Default TableProjector (TableColumn a) (IC.Column a) where
  def = tableProjector
  
-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (ViewColumnMaker a) where
  fmap f (ViewColumnMaker g) = ViewColumnMaker (fmap f g)

instance Applicative (ViewColumnMaker a) where
  pure = ViewColumnMaker . pure
  ViewColumnMaker f <*> ViewColumnMaker x = ViewColumnMaker (f <*> x)

instance Profunctor ViewColumnMaker where
  dimap f g (ViewColumnMaker q) = ViewColumnMaker (dimap f g q)

instance ProductProfunctor ViewColumnMaker where
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

instance Functor (TableProjector a) where
  fmap f (TableProjector g) = TableProjector ((fmap . fmap) f g)

instance Applicative (TableProjector a) where
  pure b = TableProjector ((const . I.Identity) b)
  TableProjector b <*> TableProjector c = TableProjector (\a -> b a <*> c a)

instance Profunctor TableProjector where
  dimap f g (TableProjector q) = TableProjector (dimap f (fmap g) q)
  
instance ProductProfunctor TableProjector where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct
  
--}
