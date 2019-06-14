{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

-- TODO
-- Updater -- easier -- this one's probably not needed
-- NullMaker -- easier
-- Constant -- harder
-- QueryRunner -- harder

module Opaleye.Internal.TypeFamilies where

import           Opaleye.Column (Column, Nullable)
import qualified Opaleye.Field as F
import qualified Data.Profunctor.Product as PP
import Data.Kind

type family IMap f a

data HT
data OT
data NullsT
data WT

type NN = 'F.NonNullable
type N  = 'F.Nullable

data Optionality = OReq | OOpt

type Req = 'OReq
type Opt = 'OOpt

type family A (a :: Arr h k1 k2) (b :: k1) :: k2

data Arr h k1 k2 where
  K     :: k1 -> Arr h k2 k1
  S     :: Arr h k1 (k2 -> k3)
        -> Arr h k1 k2
        -> Arr h k1 k3
  I     :: Arr h k1 k1
  H     :: h -> Arr h k2 k3

type (:<*>) = 'S
type Pure = 'K
type (:<$>) f = (:<*>) (Pure f)
type Id = 'I
type (:<|) f x = A f x

type instance A 'I a = a
type instance A ('K k1) _ = k1
type instance A ('S f x) a = (A f a) (A x a)

data C a = C (a, a, F.Nullability)
data TC a = TC ((a, a, F.Nullability), Optionality)

type instance A ('H HT) ('C '(h, o, NN)) = h
type instance A ('H HT) ('C '(h, o, N))  = Maybe h
type instance A ('H OT) ('C '(h, o, NN)) = Column o
type instance A ('H OT) ('C '(h, o, N))  = Column (Nullable o)
type instance A ('H NullsT) ('C '(h, o, NN)) = Column (Nullable o)

type instance A ('H HT) ('TC '(t, b)) = A ('H HT) ('C t)
type instance A ('H OT) ('TC '(t, b)) = A ('H OT) ('C t)
type instance A ('H WT) ('TC '(t, Req)) = A ('H OT) ('C t)
type instance A ('H WT) ('TC '(t, Opt)) = Maybe (A ('H OT) ('C t))
type instance A ('H NullsT) ('TC '(t, b)) = A ('H NullsT) ('C t)

type RecordField f a b c = A f ('C '(a, b, c))
type TableRecordField f a b c d = A f ('TC '( '(a, b, c), d))
-- | Do not use.  Use 'TableRecordField' instead.  Will be deprecated
-- in version 0.7.
type TableField f a b c d = TableRecordField f a b c d

type H = 'H HT
type O = 'H OT
type Nulls = 'H NullsT
type W = 'H WT
type F = 'H

class SequencePPHKD (rec :: Arr k1 k2 * -> *) where
  sequencePPHKD
    :: PP.ProductProfunctor p => rec (p :<$> (a :: Arr k1 k2 *) :<*> b)
                              -> p (rec a) (rec b)
