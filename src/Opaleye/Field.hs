{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Functions for working directly with 'Field_'s.
--
-- Please note that numeric 'Field_' types are instances of 'Num', so
-- you can use '*', '/', '+', '-' on them.  To create 'Field_'s, see
-- "Opaleye.ToFields" and "Opaleye.SqlTypes".
--
-- 'Field_' used to be called t'C.Column' and for technical reasons
-- there are still a few uses of the old name around.  If you see
-- @t'C.Column' SqlType@ then you can understand it as @'Field'
-- SqlType@, and if you see @t'C.Column' ( t'C.Nullable' SqlType )@ then
-- you can understand it as @'FieldNullable' SqlType@.
--
-- t'C.Column' will be removed in version 0.11.
--
-- (Due to Haddock formatting errors, this documentation previously
-- incorrectly stated that @Field_@ would be removed.  It won't be!)

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Opaleye.Field (
  Field_,
  Field,
  FieldNullable,
  Nullability(..),
  -- * Casting fields
  C.unsafeCast,
  unsafeCastSqlType,
  unsafeCoerceField,
  -- * Working with @NULL@
  -- | Instead of working with @NULL@ you are recommended to use
  -- "Opaleye.MaybeFields" instead.
  Opaleye.Field.null,
  typedNull,
  untypedNull,
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

import           Data.Proxy (Proxy(Proxy))

-- FIXME Put Nullspec (or sqltype?) constraint on this

-- | A @NULL@ of any type.  This will change to become 'typedNull' in
-- a future version.
null :: FieldNullable a
null = Column (HPQ.ConstExpr HPQ.NullLit)

-- | Cast a column to any other type, as long as it has an instance of
-- 'T.IsSqlType'.  Should be used in preference to 'C.unsafeCast'.
unsafeCastSqlType :: forall a b n. (T.IsSqlType b) => Field_ n a -> Field_ n b
unsafeCastSqlType = C.unsafeCast (T.showSqlType @b Proxy)

-- | Same as 'null', but with an explicit type @CAST@. This can help
-- in situations when PostgreSQL can't figure out the type of a
-- @NULL@. In a future major version this will replace @null@.
typedNull :: T.IsSqlType a => FieldNullable a
typedNull = unsafeCastSqlType null

-- | A @NULL@ of any type with no @CAST@ supplied.  Use this in
-- preference to 'null' if you really don't want a type cast applied
-- to your @NULL@.
untypedNull :: FieldNullable a
untypedNull = null

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
              -- ^ ͘
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
             -- ^ ͘
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
