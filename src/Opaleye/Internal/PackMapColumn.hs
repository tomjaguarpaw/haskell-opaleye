{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opaleye.Internal.PackMapColumn where

import qualified Opaleye.Internal.Column as IC
import           Opaleye.Internal.PackMap
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Control.Applicative (Applicative, pure, (<*>))
import qualified Data.Functor.Identity as I
import           Data.Profunctor (Profunctor, dimap, lmap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

newtype PackMapColumn f s t =
  PackMapColumn (PackMap (f HPQ.PrimExpr) HPQ.PrimExpr (f s) t)

pmColumn :: Functor f => PackMapColumn f (IC.Column s) (IC.Column t)
pmColumn = PackMapColumn (iso (fmap IC.unColumn) IC.Column)

runPMC :: Applicative f
       => (a -> g columns)
       -> (g HPQ.PrimExpr -> b)
       -> PackMapColumn g columns columns'
       -> (b -> f HPQ.PrimExpr)
       -> a
       -> f columns'
runPMC f g (PackMapColumn b) h = traversePM b (h . g) . f

instance Functor f
         => D.Default (PackMapColumn f) (IC.Column a) (IC.Column a) where
  def = pmColumn

data Pair a = Pair a a deriving Functor

unPair :: Pair a -> (a, a)
unPair (Pair x y) = (x, y)

-- {

-- Boilerplate instances

instance Functor (PackMapColumn f s) where
  fmap f (PackMapColumn g) = PackMapColumn (fmap f g)

instance Applicative (PackMapColumn f s) where
  pure x = PackMapColumn (pure x)
  PackMapColumn f <*> PackMapColumn x = PackMapColumn ((<*>) f x)

instance Functor f => Profunctor (PackMapColumn f) where
  dimap f g (PackMapColumn q) = PackMapColumn (dimap (fmap f) g q)

instance Functor f => ProductProfunctor (PackMapColumn f) where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor (PackMapColumn I.Identity) where
  PackMapColumn f +++! PackMapColumn g =
    PackMapColumn (lmap (either (Left . I.Identity) (Right . I.Identity)
                        . I.runIdentity)
                        (f PP.+++! g))

-- }
