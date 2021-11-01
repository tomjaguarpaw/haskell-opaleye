{-# LANGUAGE ConstraintKinds #-}

module Opaleye.Internal.Column where

import Data.String

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- | A column of a @Query@, of type @pgType@.  For example 'Column'
-- @SqlInt4@ is an @int4@ column and a 'Column' @SqlText@ is a @text@
-- column.
--
-- The name @Column@ will be replaced by @Field@ in version 0.9.
-- There already exists a @Field@ type family to help smooth the
-- transition.  We recommend that you use @Field_@, @Field@ or
-- @FieldNullable@ instead of @Column@ everywhere that it is
-- sufficient.
newtype Column pgType = Column HPQ.PrimExpr

-- | Only used within a 'Column', to indicate that it can be @NULL@.
-- For example, a 'Column' ('Nullable' @SqlText@) can be @NULL@ but a
-- 'Column' @SqlText@ cannot.
data Nullable a = Nullable

unColumn :: Column a -> HPQ.PrimExpr
unColumn (Column e) = e

-- | Treat a 'Column' as though it were of a different type.  If such
-- a treatment is not valid then Postgres may fail with an error at
-- SQL run time.
unsafeCoerceColumn :: Column a -> Column b
unsafeCoerceColumn (Column e) = Column e

-- | Cast a column to any other type. Implements Postgres's @::@ or
-- @CAST( ... AS ... )@ operations.  This is safe for some
-- conversions, such as uuid to text.
unsafeCast :: String -> Column a -> Column b
unsafeCast = mapColumn . HPQ.CastExpr
  where
    mapColumn :: (HPQ.PrimExpr -> HPQ.PrimExpr) -> Column c -> Column a
    mapColumn primExpr c = Column (primExpr (unColumn c))

unsafeCompositeField :: Column a -> String -> Column b
unsafeCompositeField (Column e) fieldName =
  Column (HPQ.CompositeExpr e fieldName)

binOp :: HPQ.BinOp -> Column a -> Column b -> Column c
binOp op (Column e) (Column e') = Column (HPQ.BinExpr op e e')

unOp :: HPQ.UnOp -> Column a -> Column b
unOp op (Column e) = Column (HPQ.UnExpr op e)

-- For import order reasons we can't make the return type SqlBool
unsafeCase_ :: [(Column pgBool, Column a)] -> Column a -> Column a
unsafeCase_ alts (Column otherwise_) = Column (HPQ.CaseExpr (unColumns alts) otherwise_)
  where unColumns = map (\(Column e, Column e') -> (e, e'))

unsafeIfThenElse :: Column pgBool -> Column a -> Column a -> Column a
unsafeIfThenElse cond t f = unsafeCase_ [(cond, t)] f

unsafeGt :: Column a -> Column a -> Column pgBool
unsafeGt = binOp (HPQ.:>)

unsafeEq :: Column a -> Column a -> Column pgBool
unsafeEq = binOp (HPQ.:==)

class SqlNum a where
  pgFromInteger :: Integer -> Column a
  pgFromInteger = sqlFromInteger

  sqlFromInteger :: Integer -> Column a

type PGNum = SqlNum

instance SqlNum a => Num (Column a) where
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
  pgFromRational :: Rational -> Column a
  pgFromRational = sqlFromRational

  sqlFromRational :: Rational -> Column a

type PGFractional = SqlFractional

instance (SqlNum a, SqlFractional a) => Fractional (Column a) where
  fromRational = sqlFromRational
  (/) = binOp (HPQ.:/)

-- | A dummy typeclass whose instances support integral operations.
class SqlIntegral a

type PGIntegral = SqlIntegral

class SqlString a where
    pgFromString :: String -> Column a
    pgFromString = sqlFromString

    sqlFromString :: String -> Column a

type PGString = SqlString

instance SqlString a => IsString (Column a) where
  fromString = sqlFromString
