{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-- | Functions for working directly with 'Column's.
--
-- Please note that numeric 'Column' types are instances of 'Num', so
-- you can use '*', '/', '+', '-' on them.

module Opaleye.Column (-- * 'Column'
                       Column',
                       Column,
                       NullableColumn,
                       -- * Working with @NULL@
                       Nullability(..),
                       null,
                       isNull,
                       matchNullable,
                       fromNullable,
                       toNullable,
                       maybeToNullable,
                       -- * Unsafe operations
                       unsafeCast,
                       unsafeCoerce,
                       unsafeCoerceColumn,
                       unsafeCompositeField,
                       -- * Entire module
                       module Opaleye.Column)  where

import           Opaleye.Internal.Column ( Nullability(..), unsafeCoerce, unsafeCoerceColumn
                                         , Column, NullableColumn, Column'
                                         , unsafeCast, unsafeCompositeField
                                         )
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.PGTypes as T
import           Prelude hiding (null)

-- | A NULL of any type
null :: NullableColumn a
null = C.Column (HPQ.ConstExpr HPQ.NullLit)

-- | @TRUE@ if the value of the column is @NULL@, @FALSE@ otherwise.
isNull :: NullableColumn a -> Column T.PGBool
isNull = C.unOp HPQ.OpIsNull

-- | If the @NullableColumn a@ is NULL then return the @Column n b@
-- otherwise map the underlying @NullableColumn a@ using the provided
-- function.
--
-- The Opaleye equivalent of 'Data.Maybe.maybe'.
matchNullable :: Column' n b -> (Column' m a -> Column' n b) -> NullableColumn a
              -> Column' n b
matchNullable replacement f x = C.unsafeIfThenElse (isNull x) replacement
                                                   (f (unsafeCoerceColumn x))

-- | If the @NullableColumn a@ is NULL then return the provided
-- @Column a@ otherwise return the underlying @Column a@.
--
-- The Opaleye equivalent of 'Data.Maybe.fromMaybe'.
fromNullable :: Column a -> NullableColumn a -> Column a
fromNullable = flip matchNullable unsafeCoerceColumn

-- | Treat a column as though it were nullable.  This is always safe.
--
-- The Opaleye equivalent of 'Data.Maybe.Just'.
toNullable :: Column' n a -> NullableColumn a
toNullable = unsafeCoerceColumn

-- | If the argument is 'Data.Maybe.Nothing' return NULL otherwise return the
-- provided value coerced to a nullable type.
maybeToNullable :: Maybe (Column' n a) -> NullableColumn a
maybeToNullable = maybe null toNullable
