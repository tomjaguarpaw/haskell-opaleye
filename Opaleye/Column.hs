module Opaleye.Column where

import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Database.HaskellDB.Query as Q

newtype Column a = Column PQ.PrimExpr deriving Show

unsafeCoerce :: Column a -> Column b
unsafeCoerce (Column e) = Column e

-- This may well end up moving out somewhere else
constant :: Q.ShowConstant a => a -> Column a
constant = Column . PQ.ConstExpr . Q.showConstant

binOp :: PQ.BinOp -> Column a -> Column b -> Column c
binOp op (Column e) (Column e') = Column (PQ.BinExpr op e e')

case_ :: [(Column Bool, Column a)] -> Column a -> Column a
case_ alts (Column otherwise_) = Column (PQ.CaseExpr (unColumns alts) otherwise_)
  where unColumns = map (\(Column e, Column e') -> (e, e'))

(.>) :: Column a -> Column a -> Column Bool
(.>) = binOp PQ.OpGt

(.==) :: Column a -> Column a -> Column Bool
(.==) = binOp PQ.OpEq

-- FIXME: This is missing many methods!
-- The constraints here are not really
instance (Q.ShowConstant a, Num a) => Num (Column a) where
  fromInteger = constant . fromInteger
  (*) = binOp PQ.OpMul
  (+) = binOp PQ.OpPlus
  (-) = binOp PQ.OpMinus

  abs (Column e) = Column (PQ.UnExpr (PQ.UnOpOther "@") e)
  negate (Column e) = Column (PQ.UnExpr (PQ.UnOpOther "-") e)

  -- We can't use Postgres's 'sign' function because it returns only a
  -- numeric or a double
  signum c = case_ [((c .> 0), 1), ((c .== 0), 0)] (-1)

instance (Q.ShowConstant a, Fractional a) => Fractional (Column a) where
  fromRational = constant . fromRational
  (/) = binOp PQ.OpDiv
