{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Join where

import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PackMap as PM
import           Opaleye.Internal.Column (Column, Nullable)
import qualified Opaleye.Column as C

import qualified Control.Applicative as A

import           Data.Profunctor (Profunctor, dimap)
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

newtype NullMaker a b = NullMaker (a -> b)

toNullable :: NullMaker a b -> a -> b
toNullable (NullMaker f) = f

extractLeftJoinFields :: Int -> T.Tag -> HPQ.PrimExpr
            -> PM.PM [(HPQ.Symbol, HPQ.PrimExpr)] HPQ.PrimExpr
extractLeftJoinFields n = PM.extractAttr ("result" ++ show n ++ "_")

instance D.Default NullMaker (Column a) (Column (Nullable a)) where
  def = NullMaker C.unsafeCoerceColumn

instance D.Default NullMaker (Column (Nullable a)) (Column (Nullable a)) where
  def = NullMaker C.unsafeCoerceColumn

-- { Boilerplate instances

instance Functor (NullMaker a) where
  fmap f (NullMaker g) = NullMaker (fmap f g)

instance A.Applicative (NullMaker a) where
  pure = NullMaker . A.pure
  NullMaker f <*> NullMaker x = NullMaker (f A.<*> x)

instance Profunctor NullMaker where
  dimap f g (NullMaker h) = NullMaker (dimap f g h)

instance PP.ProductProfunctor NullMaker where
  empty  = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

--
