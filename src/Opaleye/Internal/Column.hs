module Opaleye.Internal.Column where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- | The 'Num' and 'Fractional' instances for 'Column' 'a' are too
-- general.  For example, they allow you to add two 'Column'
-- 'String's.  This will be fixed in a subsequent release.
newtype Column a = Column HPQ.PrimExpr deriving Show

data Nullable a = Nullable

unColumn :: Column a -> HPQ.PrimExpr
unColumn (Column e) = e

{-# DEPRECATED unsafeCoerce "Use unsafeCoerceColumn instead" #-}
unsafeCoerce :: Column a -> Column b
unsafeCoerce = unsafeCoerceColumn

unsafeCoerceColumn :: Column a -> Column b
unsafeCoerceColumn (Column e) = Column e

binOp :: HPQ.BinOp -> Column a -> Column b -> Column c
binOp op (Column e) (Column e') = Column (HPQ.BinExpr op e e')

unOp :: HPQ.UnOp -> Column a -> Column b
unOp op (Column e) = Column (HPQ.UnExpr op e)

-- For import order reasons we can't make the return type PGBool
unsafeCase_ :: [(Column pgBool, Column a)] -> Column a -> Column a
unsafeCase_ alts (Column otherwise_) = Column (HPQ.CaseExpr (unColumns alts) otherwise_)
  where unColumns = map (\(Column e, Column e') -> (e, e'))

unsafeIfThenElse :: Column pgBool -> Column a -> Column a -> Column a
unsafeIfThenElse cond t f = unsafeCase_ [(cond, t)] f

unsafeGt :: Column a -> Column a -> Column pgBool
unsafeGt = binOp HPQ.OpGt

unsafeEq :: Column a -> Column a -> Column pgBool
unsafeEq = binOp HPQ.OpEq

class PGNum a where
  pgFromInteger :: Integer -> Column a

instance PGNum a => Num (Column a) where
  fromInteger = pgFromInteger
  (*) = binOp HPQ.OpMul
  (+) = binOp HPQ.OpPlus
  (-) = binOp HPQ.OpMinus

  abs = unOp HPQ.OpAbs
  negate = unOp HPQ.OpNegate

  -- We can't use Postgres's 'sign' function because it returns only a
  -- numeric or a double
  signum c = unsafeCase_ [(c `unsafeGt` 0, 1), (c `unsafeEq` 0, 0)] (-1)

class PGFractional a where
  pgFromRational :: Rational -> Column a

instance (PGNum a, PGFractional a) => Fractional (Column a) where
  fromRational = pgFromRational
  (/) = binOp HPQ.OpDiv
