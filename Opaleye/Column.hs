module Opaleye.Column where

import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Database.HaskellDB.Query as Q

newtype Column a = Column PQ.PrimExpr deriving Show

-- This may well end up moving out somewhere else
constant :: Q.ShowConstant a => a -> Column a
constant = Column . PQ.ConstExpr . Q.showConstant

-- FIXME: This is missing many methods!
instance (Q.ShowConstant a, Num a) => Num (Column a) where
  fromInteger = constant . fromInteger
