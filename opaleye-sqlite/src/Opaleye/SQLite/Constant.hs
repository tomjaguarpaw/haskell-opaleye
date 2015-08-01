{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.SQLite.Constant where

import           Opaleye.SQLite.Column           (Column)
import qualified Opaleye.SQLite.Column           as C
import qualified Opaleye.SQLite.SqlTypes         as T

import qualified Data.Text                       as ST
import qualified Data.Text.Lazy                  as LT

import qualified Data.Profunctor.Product         as PP
import           Data.Profunctor.Product         (empty, (***!), (+++!))
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor                 as P

import           Control.Applicative (Applicative, pure, (<*>))


newtype Constant haskells columns =
  Constant { constantExplicit :: haskells -> columns }

constant :: D.Default Constant haskells columns
         => haskells -> columns
constant = constantExplicit D.def

instance D.Default Constant haskell (Column sql)
         => D.Default Constant (Maybe haskell) (Column (C.Nullable sql)) where
  def = Constant (C.maybeToNullable . fmap f)
    where Constant f = D.def

instance D.Default Constant Int (Column T.SqlInt) where
  def = Constant T.sqlInt

instance D.Default Constant String (Column T.SqlText) where
  def = Constant T.sqlString

instance D.Default Constant ST.Text (Column T.SqlText) where
  def = Constant T.sqlStrictText

instance D.Default Constant LT.Text (Column T.SqlText) where
  def = Constant T.sqlLazyText

instance D.Default Constant Double (Column T.SqlReal) where
  def = Constant T.sqlReal

instance D.Default Constant Bool (Column T.SqlBool) where
  def = Constant T.sqlBool


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
