{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Opaleye.Internal.Column where

import Data.String

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

data Nullability = NonNullable | Nullable

-- | A field of a @Select@, of type @sqlType@.  For example a @Field
-- SqlInt4@ is an @int4@ column and a @Field SqlText@ is a @text@
-- column.
newtype Field_ (n :: Nullability) sqlType = Column HPQ.PrimExpr

type Field = Field_ NonNullable
type FieldNullable = Field_ 'Nullable

-- | Only used within a 'Column', to indicate that it can be @NULL@.
-- For example, a 'Column' ('Nullable' @SqlText@) can be @NULL@ but a
-- 'Column' @SqlText@ cannot.
data Nullable a = Nullable_

-- | Do not use. Use 'Field' instead.  Will be removed in a later
-- version.
type family Column a where
  Column (Nullable a) = FieldNullable a
  Column a = Field a

unColumn :: Field_ n a -> HPQ.PrimExpr
unColumn (Column e) = e

-- | Treat a 'Column' as though it were of a different type.  If such
-- a treatment is not valid then Postgres may fail with an error at
-- SQL run time.
unsafeCoerceColumn :: Field_ n a -> Field_ n' b
unsafeCoerceColumn (Column e) = Column e

-- | Cast a column to any other type. Implements Postgres's @::@ or
-- @CAST( ... AS ... )@ operations.  This is safe for some
-- conversions, such as uuid to text.
unsafeCast :: String -> Field_ n a -> Field_ n b
unsafeCast = mapColumn . HPQ.CastExpr
  where
    mapColumn :: (HPQ.PrimExpr -> HPQ.PrimExpr) -> Field_ n c -> Field_ n' a
    mapColumn primExpr c = Column (primExpr (unColumn c))

unsafeCompositeField :: Field_ n a -> String -> Field_ n' b
unsafeCompositeField (Column e) fieldName =
  Column (HPQ.CompositeExpr e fieldName)

unsafeFromNullable :: Field_ n a
                   -> Field_ n' a
unsafeFromNullable (Column e) = Column e

binOp :: HPQ.BinOp -> Field_ n a -> Field_ n' b -> Field_ n'' c
binOp op (Column e) (Column e') = Column (HPQ.BinExpr op e e')

unOp :: HPQ.UnOp -> Field_ n a -> Field_ n' b
unOp op (Column e) = Column (HPQ.UnExpr op e)

-- For import order reasons we can't make the argument type SqlBool
unsafeCase_ :: [(Field_ n pgBool, Field_ n' a)] -> Field_ n' a -> Field_ n' a
unsafeCase_ alts (Column otherwise_) = Column (HPQ.CaseExpr (unColumns alts) otherwise_)
  where unColumns = map (\(Column e, Column e') -> (e, e'))

unsafeIfThenElse :: Field_ n' pgBool -> Field_ n a -> Field_ n a -> Field_ n a
unsafeIfThenElse cond t f = unsafeCase_ [(cond, t)] f

unsafeGt :: Field_ n a -> Field_ n a -> Field_ n' pgBool
unsafeGt = binOp (HPQ.:>)

unsafeEq :: Field_ n a -> Field_ n a -> Field_ n' pgBool
unsafeEq = binOp (HPQ.:==)

class SqlNum a where
  pgFromInteger :: Integer -> Field a
  pgFromInteger = sqlFromInteger

  sqlFromInteger :: Integer -> Field a

type PGNum = SqlNum

instance SqlNum a => Num (Field a) where
  fromInteger = pgFromInteger
  (*) = binOp (HPQ.:*)
  (+) = binOp (HPQ.:+)
  (-) = binOp (HPQ.:-)

  abs = unOp HPQ.OpAbs
  negate = unOp HPQ.OpNegate

  -- We can't use Postgres's 'sign' function because it returns only a
  -- numeric or a double
  signum c = unsafeCase_ [(c `unsafeGt` 0, 1), (c `unsafeEq` 0, 0)] (-1)

class SqlFractional a where
  pgFromRational :: Rational -> Field a
  pgFromRational = sqlFromRational

  sqlFromRational :: Rational -> Field a

type PGFractional = SqlFractional

instance (SqlNum a, SqlFractional a) => Fractional (Field a) where
  fromRational = sqlFromRational
  (/) = binOp (HPQ.:/)

-- | A dummy typeclass whose instances support integral operations.
class SqlIntegral a

type PGIntegral = SqlIntegral

class SqlString a where
    pgFromString :: String -> Field a
    pgFromString = sqlFromString

    sqlFromString :: String -> Field a

type PGString = SqlString

instance SqlString a => IsString (Field a) where
  fromString = sqlFromString
