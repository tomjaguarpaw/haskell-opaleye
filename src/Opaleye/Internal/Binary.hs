{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Internal.Binary where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PackMap as PM

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))
import           Control.Arrow ((***))

extractBinaryFields :: T.Tag -> (HPQ.PrimExpr, HPQ.PrimExpr)
                    -> PM.PM [(HPQ.Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))]
                             HPQ.PrimExpr
extractBinaryFields = PM.extractAttr "binary"

newtype Binaryspec columns columns' =
  Binaryspec (PM.PackMap (HPQ.PrimExpr, HPQ.PrimExpr) HPQ.PrimExpr
                         (columns, columns) columns')

runBinaryspec :: Applicative f => Binaryspec columns columns'
                 -> ((HPQ.PrimExpr, HPQ.PrimExpr) -> f HPQ.PrimExpr)
                 -> (columns, columns) -> f columns'
runBinaryspec (Binaryspec b) = PM.traversePM b

binaryspecColumn :: Binaryspec (Column a) (Column a)
binaryspecColumn = Binaryspec (PM.PackMap (\f (Column e, Column e')
                                           -> fmap Column (f (e, e'))))

instance Default Binaryspec (Column a) (Column a) where
  def = binaryspecColumn

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (Binaryspec a) where
  fmap f (Binaryspec g) = Binaryspec (fmap f g)

instance Applicative (Binaryspec a) where
  pure = Binaryspec . pure
  Binaryspec f <*> Binaryspec x = Binaryspec (f <*> x)

instance Profunctor Binaryspec where
  dimap f g (Binaryspec b) = Binaryspec (dimap (f *** f) g b)

instance ProductProfunctor Binaryspec where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

-- }
