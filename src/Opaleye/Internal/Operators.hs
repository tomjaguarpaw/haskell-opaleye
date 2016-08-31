{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.Operators where

import           Opaleye.Internal.Column (Column)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.PGTypes as T

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product.Default as D

infix 4 .==
(.==) :: forall columns. D.Default EqPP columns columns
      => columns -> columns -> Column T.PGBool
(.==) = eqExplicit (D.def :: EqPP columns columns)

infixr 3 .&&
(.&&) :: Column T.PGBool -> Column T.PGBool -> Column T.PGBool
(.&&) = C.binOp HPQ.OpAnd

-- Probably should be newtype
data EqPP a b = EqPP (a -> a -> Column T.PGBool)

eqExplicit :: EqPP columns a -> columns -> columns -> Column T.PGBool
eqExplicit (EqPP f) = f

instance D.Default EqPP (Column a) (Column a) where
  def = EqPP C.unsafeEq

-- { Boilerplate instances

instance Profunctor EqPP where
  dimap f _ (EqPP h) = EqPP (\a a' -> h (f a) (f a'))

instance ProductProfunctor EqPP where
  empty = EqPP (\() () -> T.pgBool True)
  EqPP f ***! EqPP f' = EqPP (\a a' ->
                               f (fst a) (fst a') .&& f' (snd a) (snd a'))

-- }
