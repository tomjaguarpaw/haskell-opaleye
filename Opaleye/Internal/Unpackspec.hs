{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.Unpackspec where

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Column as C

import           Control.Applicative (Applicative, pure, (<*>))
import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP

import qualified Database.HaskellDB.PrimQuery as PQ

newtype Unpackspec columns b = Unpackspec (PM.PackMap PQ.PrimExpr () columns b)

unpackspecColumn :: Unpackspec (C.Column a) (C.Column a)
unpackspecColumn = Unpackspec
                   (PM.PackMap (\f c@(C.Column pe) -> fmap (const c) (f pe)))

runUnpackspec :: Applicative f
                 => Unpackspec columns b
                 -> (PQ.PrimExpr -> f ())
                 -> (columns -> f b)
runUnpackspec (Unpackspec f) = PM.packmap f

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
