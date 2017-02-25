{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opaleye.Internal.Manipulation where

import qualified Control.Applicative as A

import           Opaleye.Internal.Column (Column)
import           Data.Profunctor                 (Profunctor, dimap)
import qualified Data.Profunctor.Product         as PP
import qualified Data.Profunctor.Product.Default as D

newtype Updater a b = Updater (a -> b)

-- { Boilerplate instances

instance Functor (Updater a) where
  fmap f (Updater g) = Updater (fmap f g)

instance A.Applicative (Updater a) where
  pure = Updater . A.pure
  Updater f <*> Updater x = Updater (f A.<*> x)

instance Profunctor Updater where
  dimap f g (Updater h) = Updater (dimap f g h)

instance PP.ProductProfunctor Updater where
  empty  = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

--

instance D.Default Updater (Column a) (Column a) where
  def = Updater id

instance D.Default Updater (Column a) (Maybe (Column a)) where
  def = Updater Just

