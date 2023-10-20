-- | Functions for working directly with 'Field_'s.
--
-- Please note that numeric 'Field_' types are instances of 'Num', so
-- you can use '*', '/', '+', '-' on them.  To create 'Field_'s, see
-- "Opaleye.ToFields" and "Opaleye.SqlTypes".
--
-- 'Field_' used to be called 'C.Column' and for technical reasons
-- there are still a few uses of the old name around.  If you see
-- @'C.Column' SqlType@ then you can understand it as @'Field'
-- SqlType@, and if you see @'C.Column' ('C.Nullable' SqlType)@ then
-- you can understand it as @'FieldNullable' SqlType@.
--
-- 'C.Column' will be removed in version 0.11.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Opaleye.Field (
  Field_,
  Field,
  FieldNullable,
  Nullability(..),
  -- * Coercing fields
  C.unsafeCast,
  unsafeCoerceField,
  -- * Working with @NULL@
  -- | Instead of working with @NULL@ you are recommended to use
  -- "Opaleye.MaybeFields" instead.
  Opaleye.Field.null,
  isNull,
  matchNullable,
  fromNullable,
  toNullable,
  maybeToNullable,
  ) where

import           Prelude hiding (null)

import           Opaleye.Internal.Column
  (Field_(Column), FieldNullable, Field, Nullability(NonNullable, Nullable))
import qualified Opaleye.Internal.Column   as C
import qualified Opaleye.Internal.PGTypesExternal  as T
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- FIXME Put Nullspec (or sqltype?) constraint on this

-- | A NULL of any type
null :: FieldNullable a
null = Column (HPQ.ConstExpr HPQ.NullLit)

-- | @TRUE@ if the value of the field is @NULL@, @FALSE@ otherwise.
isNull :: FieldNullable a -> Field T.PGBool
isNull = C.unOp HPQ.OpIsNull

-- | If the @Field 'Nullable a@ is NULL then return the @Field
-- 'NonNullable b@ otherwise map the underlying @Field 'Nullable a@
-- using the provided function.
--
-- The Opaleye equivalent of 'Data.Maybe.maybe'.
matchNullable :: Field b
              -- ^
              -> (Field a -> Field b)
              -- ^
              -> FieldNullable a
              -- ^
              -> Field b
matchNullable replacement f x = C.unsafeIfThenElse (isNull x) replacement
                                                   (f (unsafeCoerceField x))

-- | If the @FieldNullable a@ is NULL then return the provided
-- @Field a@ otherwise return the underlying @Field
-- a@.
--
-- The Opaleye equivalent of 'Data.Maybe.fromMaybe' and very similar
-- to PostgreSQL's @COALESCE@.
fromNullable :: Field a
             -- ^
             -> FieldNullable a
             -- ^
             -> Field a
fromNullable = flip matchNullable id

-- | Treat a field as though it were nullable.  This is always safe.
--
-- The Opaleye equivalent of 'Data.Maybe.Just'.
toNullable :: Field a -> FieldNullable a
toNullable = C.unsafeCoerceColumn

-- | If the argument is 'Data.Maybe.Nothing' return NULL otherwise return the
-- provided value coerced to a nullable type.
maybeToNullable :: Maybe (Field a)
                -> FieldNullable a
maybeToNullable = maybe null toNullable

unsafeCoerceField :: Field_ n a -> Field_ n' b
unsafeCoerceField = C.unsafeCoerceColumn
