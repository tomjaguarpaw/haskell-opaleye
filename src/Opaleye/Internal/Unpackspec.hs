{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.Unpackspec where

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Column as C

import           Control.Applicative (Applicative, pure, (<*>))
import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

newtype Unpackspec columns columns' =
  Unpackspec (PM.PackMap HPQ.PrimExpr HPQ.PrimExpr columns columns')

unpackspecColumn :: Unpackspec (C.Column a) (C.Column a)
unpackspecColumn = Unpackspec
                   (PM.PackMap (\f (IC.Column pe) -> fmap IC.Column (f pe)))

runUnpackspec :: Applicative f
                 => Unpackspec columns b
                 -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                 -> columns -> f b
runUnpackspec (Unpackspec f) = PM.traverse f

collectPEs :: Unpackspec s t -> s -> [HPQ.PrimExpr]
collectPEs unpackspec = fst . runUnpackspec unpackspec f
  where f pe = ([pe], pe)

instance D.Default Unpackspec (C.Column a) (C.Column a) where
  def = unpackspecColumn

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (Unpackspec a) where
  fmap f (Unpackspec g) = Unpackspec (fmap f g)

instance Applicative (Unpackspec a) where
  pure = Unpackspec . pure
  Unpackspec f <*> Unpackspec x = Unpackspec (f <*> x)

instance Profunctor Unpackspec where
  dimap f g (Unpackspec q) = Unpackspec (dimap f g q)

instance ProductProfunctor Unpackspec where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor Unpackspec where
  Unpackspec x1 +++! Unpackspec x2 = Unpackspec (x1 PP.+++! x2)

--}
