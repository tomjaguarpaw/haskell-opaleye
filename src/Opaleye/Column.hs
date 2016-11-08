-- | Functions for working directly with 'Column's.
--
-- Please note that numeric 'Column' types are instances of 'Num', so
-- you can use '*', '/', '+', '-' on them.

module Opaleye.Column (-- * 'Column'
                       Column,
                       -- * Working with @NULL@
                       Nullable,
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

import           Opaleye.Internal.Column (Column, Nullable, unsafeCoerce, unsafeCoerceColumn,
                                          unsafeCast, unsafeCompositeField)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.PGTypes as T
import           Prelude hiding (null)

-- | A NULL of any type
null :: Column (Nullable a)
null = C.Column (HPQ.ConstExpr HPQ.NullLit)

-- | @TRUE@ if the value of the column is @NULL@, @FALSE@ otherwise.
isNull :: Column (Nullable a) -> Column T.PGBool
isNull = C.unOp HPQ.OpIsNull

-- | If the @Column (Nullable a)@ is NULL then return the @Column b@
-- otherwise map the underlying @Column a@ using the provided
-- function.
--
-- The Opaleye equivalent of 'Data.Maybe.maybe'.
matchNullable :: Column b -> (Column a -> Column b) -> Column (Nullable a)
              -> Column b
matchNullable replacement f x = C.unsafeIfThenElse (isNull x) replacement
                                                   (f (unsafeCoerceColumn x))

-- | If the @Column (Nullable a)@ is NULL then return the provided
-- @Column a@ otherwise return the underlying @Column a@.
--
-- The Opaleye equivalent of 'Data.Maybe.fromMaybe'.
fromNullable :: Column a -> Column (Nullable a) -> Column a
fromNullable = flip matchNullable id

-- | Treat a column as though it were nullable.  This is always safe.
--
-- The Opaleye equivalent of 'Data.Maybe.Just'.
toNullable :: Column a -> Column (Nullable a)
toNullable = unsafeCoerceColumn

-- | If the argument is 'Data.Maybe.Nothing' return NULL otherwise return the
-- provided value coerced to a nullable type.
maybeToNullable :: Maybe (Column a) -> Column (Nullable a)
maybeToNullable = maybe null toNullable
