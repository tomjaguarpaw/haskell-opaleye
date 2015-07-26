{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.SQLite.Internal.Join where

import qualified Opaleye.SQLite.Internal.Tag as T
import qualified Opaleye.SQLite.Internal.PackMap as PM
import           Opaleye.SQLite.Internal.Column (Column, Nullable)
import qualified Opaleye.SQLite.Column as C

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.SQLite.Internal.HaskellDB.PrimQuery as HPQ

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

instance Profunctor NullMaker where
  dimap f g (NullMaker h) = NullMaker (dimap f g h)

instance ProductProfunctor NullMaker where
  empty = NullMaker empty
  NullMaker f ***! NullMaker f' = NullMaker (f ***! f')

--
