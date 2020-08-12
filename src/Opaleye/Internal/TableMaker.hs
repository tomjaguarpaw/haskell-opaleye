{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.TableMaker where

import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.Unpackspec as U

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ


-- If we switch to a more lens-like approach to PackMap this should be
-- the equivalent of a Setter
newtype ViewColumnMaker strings columns =
  ViewColumnMaker (PM.PackMap () () strings columns)

runViewColumnMaker :: ViewColumnMaker strings tablecolumns ->
                       strings -> tablecolumns
runViewColumnMaker (ViewColumnMaker f) = PM.overPM f id

{-# DEPRECATED ColumnMaker "Use Unpackspec instead" #-}
type ColumnMaker = U.Unpackspec

{-# DEPRECATED runColumnMaker "Use runUnpackspec instead" #-}
runColumnMaker :: Applicative f
                  => ColumnMaker tablecolumns columns
                  -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                  -> tablecolumns -> f columns
runColumnMaker = U.runUnpackspec

-- There's surely a way of simplifying this implementation
tableColumn :: ViewColumnMaker String (C.Column a)
tableColumn = ViewColumnMaker
              (PM.PackMap (\f s -> fmap (const (mkColumn s)) (f ())))
  where mkColumn = IC.Column . HPQ.BaseTableAttrExpr

instance Default ViewColumnMaker String (C.Column a) where
  def = tableColumn

{-# DEPRECATED column "Use unpackspecColumn instead" #-}
column :: ColumnMaker (C.Column a) (C.Column a)
column = U.unpackspecField

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
  purePP = pure
  (****) = (<*>)

--}
