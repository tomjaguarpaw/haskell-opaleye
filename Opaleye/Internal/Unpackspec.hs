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

import qualified Database.HaskellDB.PrimQuery as PQ

newtype Unpackspec columns columns' =
  Unpackspec (PM.PackMap PQ.PrimExpr PQ.PrimExpr columns columns')

unpackspecColumn :: Unpackspec (C.Column a) (C.Column a)
unpackspecColumn = Unpackspec
                   (PM.PackMap (\f (IC.Column pe) -> fmap IC.Column (f pe)))

runUnpackspec :: Applicative f
                 => Unpackspec columns b
                 -> (PQ.PrimExpr -> f PQ.PrimExpr)
                 -> columns -> f b
runUnpackspec (Unpackspec f) = PM.packmap f

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

--}
