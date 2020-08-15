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
-- 'C.Column' will be fully deprecated in version 0.8.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Opaleye.Field (
  Field_,
  Nullability(..),
  FieldNullable,
  Field,
  Opaleye.Field.null,
  isNull,
  matchNullable,
  fromNullable,
  toNullable,
  maybeToNullable,
  unsafeCoerceField
  ) where

import qualified Opaleye.Column   as C
import qualified Opaleye.Internal.PGTypesExternal  as T

-- | The name @Column@ will be replaced by @Field@ in version 0.8.
-- The @Field_@, @Field@ and @FieldNullable@ types exist to help
-- smooth the transition.  We recommend that you use @Field_@, @Field@
-- or @FieldNullable@ instead of @Column@ everywhere that it is
-- sufficient.
type family Field_ (a :: Nullability) b

data Nullability = NonNullable | Nullable

type instance Field_ 'NonNullable a = C.Column a
type instance Field_ 'Nullable a = C.Column (C.Nullable a)

type FieldNullable  a = Field_ 'Nullable a
type Field a = Field_ 'NonNullable a

-- | A NULL of any type
null :: FieldNullable a
null = C.null

-- | @TRUE@ if the value of the field is @NULL@, @FALSE@ otherwise.
isNull :: FieldNullable a -> Field T.PGBool
isNull = C.isNull

-- | If the @Field 'Nullable a@ is NULL then return the @Field
-- 'NonNullable b@ otherwise map the underlying @Field 'Nullable a@
-- using the provided function.
--
-- The Opaleye equivalent of 'Data.Maybe.maybe'.
--
-- Will be generalized to @Field_ n b@ in a later version.
matchNullable :: Field_ 'NonNullable b
              -- ^
              -> (Field_ 'NonNullable a -> Field_ 'NonNullable b)
              -- ^
              -> Field_ 'Nullable a
              -- ^
              -> Field_ 'NonNullable b
matchNullable = C.matchNullable

-- | If the @Field 'Nullable a@ is NULL then return the provided
-- @Field 'NonNullable a@ otherwise return the underlying @Field
-- 'NonNullable a@.
--
-- The Opaleye equivalent of 'Data.Maybe.fromMaybe' and very similar
-- to PostgreSQL's @COALESCE@.
--
-- Will be generalized to @Field_ n a@ in a later version.
fromNullable :: Field_ 'NonNullable a
             -- ^
             -> Field_ 'Nullable a
             -- ^
             -> Field_ 'NonNullable a
fromNullable = C.fromNullable

-- | Treat a field as though it were nullable.  This is always safe.
--
-- The Opaleye equivalent of 'Data.Maybe.Just'.
--
-- Will be generalized to @Field_ n a@ in a later version.
toNullable :: Field_ 'NonNullable a -> Field_ 'Nullable a
toNullable = C.unsafeCoerceColumn

-- | If the argument is 'Data.Maybe.Nothing' return NULL otherwise return the
-- provided value coerced to a nullable type.
--
-- Will be generalized to @Maybe (Field_ n a)@ in a later version.
maybeToNullable :: Maybe (Field_ 'NonNullable a)
                -> Field_ 'Nullable a
maybeToNullable = C.maybeToNullable

unsafeCoerceField :: C.Column a -> C.Column b
unsafeCoerceField = C.unsafeCoerceColumn
