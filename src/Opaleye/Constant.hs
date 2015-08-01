{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Constant where

import           Opaleye.Column (Column)

import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product (empty, (***!), (+++!))
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor as P

import           Control.Applicative (Applicative, pure, (<*>))


newtype Constant haskell sql = Constant (haskell -> sql)

class ConstantColumn a b where
  constantColumn :: a -> Column b



-- { Boilerplate instances

instance Functor (Constant a) where
  fmap f (Constant g) = Constant (fmap f g)

instance Applicative (Constant a) where
  pure = Constant . pure
  Constant f <*> Constant x = Constant (f <*> x)

instance P.Profunctor Constant where
  dimap f g (Constant h) = Constant (P.dimap f g h)

instance PP.ProductProfunctor Constant where
  empty = Constant empty
  Constant f ***! Constant g = Constant (f ***! g)

instance PP.SumProfunctor Constant where
  Constant f +++! Constant g = Constant (f +++! g)

-- }
