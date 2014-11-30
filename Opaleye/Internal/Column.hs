module Opaleye.Internal.Column where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.HaskellDB.Query as Q

import           GHC.Int (Int64)

-- | The 'Num' and 'Fractional' instances for 'Column' 'a' are too
-- general.  For example, they allow you to add two 'Column'
-- 'String's.  This will be fixed in a subsequent release.
newtype Column a = Column HPQ.PrimExpr deriving Show

data Nullable a = Nullable

unColumn :: Column a -> HPQ.PrimExpr
unColumn (Column e) = e

unsafeCoerce :: Column a -> Column b
unsafeCoerce (Column e) = Column e

-- This may well end up moving out somewhere else
constant :: Q.ShowConstant a => a -> Column a
constant = Column . HPQ.ConstExpr . Q.showConstant

binOp :: HPQ.BinOp -> Column a -> Column b -> Column c
binOp op (Column e) (Column e') = Column (HPQ.BinExpr op e e')

unOp :: HPQ.UnOp -> Column a -> Column b
unOp op (Column e) = Column (HPQ.UnExpr op e)

case_ :: [(Column Bool, Column a)] -> Column a -> Column a
case_ alts (Column otherwise_) = Column (HPQ.CaseExpr (unColumns alts) otherwise_)
  where unColumns = map (\(Column e, Column e') -> (e, e'))

ifThenElse :: Column Bool -> Column a -> Column a -> Column a
ifThenElse cond t f = case_ [(cond, t)] f

(.>) :: Column a -> Column a -> Column Bool
(.>) = binOp HPQ.OpGt

(.==) :: Column a -> Column a -> Column Bool
(.==) = binOp HPQ.OpEq

-- Naughty orphan instance
instance Q.ShowConstant Int64 where
  showConstant = HPQ.IntegerLit . fromIntegral

-- The constraints here are not really appropriate.  There should be
-- some restriction to a numeric Postgres type
instance (Q.ShowConstant a, Num a) => Num (Column a) where
  fromInteger = constant . fromInteger
  (*) = binOp HPQ.OpMul
  (+) = binOp HPQ.OpPlus
  (-) = binOp HPQ.OpMinus

  abs (Column e) = Column (HPQ.UnExpr (HPQ.UnOpOther "@") e)
  negate (Column e) = Column (HPQ.UnExpr (HPQ.UnOpOther "-") e)

  -- We can't use Postgres's 'sign' function because it returns only a
  -- numeric or a double
  signum c = case_ [(c .> 0, 1), (c .== 0, 0)] (-1)

instance (Q.ShowConstant a, Fractional a) => Fractional (Column a) where
  fromRational = constant . fromRational
  (/) = binOp HPQ.OpDiv
