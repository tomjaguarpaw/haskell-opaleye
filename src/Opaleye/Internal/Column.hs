module Opaleye.Internal.Column where

import Data.String

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- | A column of a @Query@, of type @pgType@.  For example 'Column'
-- 'NonNullable' @PGInt4@ is an @int4@ column and a 'Column'
-- 'NonNullable' @PGText@ is a @text@ column.
--
-- Do not use the 'Show' instance of 'Column'.  It will be deprecated
-- in version 0.6.
newtype Column' (n :: Nullability) pgType = Column HPQ.PrimExpr deriving Show

type Column = Column' 'NonNullable
type NullableColumn = Column' 'Nullable

-- | Only used within a 'Column', to indicate that it can be @NULL@.
-- For example, a 'Column' 'Nullable' @PGText@ can be @NULL@ but a
-- 'Column' 'NonNullable' @PGText@ cannot.
data Nullability
  = Nullable
  | NonNullable

unColumn :: Column' a b -> HPQ.PrimExpr
unColumn (Column e) = e

{-# DEPRECATED unsafeCoerce "Will be removed in version 0.6.  Use unsafeCoerceColumn instead." #-}
unsafeCoerce :: Column' a b -> Column' a' b'
unsafeCoerce = unsafeCoerceColumn

-- | Treat a 'Column' as though it were of a different type.  If such
-- a treatment is not valid then Postgres may fail with an error at
-- SQL run time.
unsafeCoerceColumn :: Column' a b -> Column' a' b'
unsafeCoerceColumn (Column e) = Column e

-- | Cast a column to any other type. Implements Postgres's @::@ or
-- @CAST( ... AS ... )@ operations.  This is safe for some
-- conversions, such as uuid to text.
unsafeCast :: String -> Column' a b -> Column' a b'
unsafeCast = mapColumn . HPQ.CastExpr
  where
    mapColumn :: (HPQ.PrimExpr -> HPQ.PrimExpr) -> Column' a b -> Column' a b'
    mapColumn primExpr c = Column (primExpr (unColumn c))

unsafeCompositeField :: Column' a b -> String -> Column' a b'
unsafeCompositeField (Column e) fieldName =
  Column (HPQ.CompositeExpr e fieldName)

binOp :: HPQ.BinOp -> Column' a b -> Column' c d -> Column' x y
binOp op (Column e) (Column e') = Column (HPQ.BinExpr op e e')

unOp :: HPQ.UnOp -> Column' a b -> Column' c d
unOp op (Column e) = Column (HPQ.UnExpr op e)

-- For import order reasons we can't make the return type PGBool
unsafeCase_ :: [(Column' a pgBool, Column' c d)] -> Column' c d -> Column' c d
unsafeCase_ alts (Column otherwise_) = Column (HPQ.CaseExpr (unColumns alts) otherwise_)
  where unColumns = map (\(Column e, Column e') -> (e, e'))

unsafeIfThenElse :: Column' a pgBool -> Column' c d -> Column' c d -> Column' c d
unsafeIfThenElse cond t f = unsafeCase_ [(cond, t)] f

unsafeGt :: Column' a b -> Column' a b -> Column' n pgBool
unsafeGt = binOp (HPQ.:>)

unsafeEq :: Column' a b -> Column' a b -> Column' n pgBool
unsafeEq = binOp (HPQ.:==)

class PGNum a where
  pgFromInteger :: Integer -> Column a

instance PGNum a => Num (Column a) where
  fromInteger = pgFromInteger
  (*) = binOp (HPQ.:*)
  (+) = binOp (HPQ.:+)
  (-) = binOp (HPQ.:-)

  abs = unOp HPQ.OpAbs
  negate = unOp HPQ.OpNegate

  -- We can't use Postgres's 'sign' function because it returns only a
  -- numeric or a double
  signum c = unsafeCase_ [(c `unsafeGt` 0, 1), (c `unsafeEq` 0, 0)] (-1)

class PGFractional a where
  pgFromRational :: Rational -> Column a

instance (PGNum a, PGFractional a) => Fractional (Column a) where
  fromRational = pgFromRational
  (/) = binOp (HPQ.:/)

-- | A dummy typeclass whose instances support integral operations.
class PGIntegral a

class PGString a where
    pgFromString :: String -> Column a

instance PGString a => IsString (Column a) where
  fromString = pgFromString
