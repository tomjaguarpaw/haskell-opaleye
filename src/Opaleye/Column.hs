{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-- | Functions for working directly with 'Column's.
--
-- Please note that numeric 'Column' types are instances of 'Num', so
-- you can use '*', '/', '+', '-' on them.

module Opaleye.Column {-# DEPRECATED "Use \"Opaleye.Field\" instead.  Will be removed in version 0.11." #-}
                      (-- * 'Column'
                       Column,
                       -- * Working with @NULL@
                       Nullable,
                       null,
                       isNull,
                       -- * Unsafe operations
                       unsafeCast,
                       unsafeCoerceColumn,
                       unsafeCompositeField,
                       -- * Entire module
                       module Opaleye.Column)  where

import           Opaleye.Internal.Column (Column, Nullable, unsafeCoerceColumn,
                                          unsafeCast, unsafeCompositeField)
import qualified Opaleye.Field as F
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PGTypesExternal as T
import           Prelude hiding (null)

-- | A NULL of any type
null :: Column (Nullable a)
null = F.null

-- | @TRUE@ if the value of the column is @NULL@, @FALSE@ otherwise.
isNull :: Column (Nullable a) -> Column T.PGBool
isNull = C.unOp HPQ.OpIsNull

joinNullable :: Column (Nullable (Nullable a)) -> Column (Nullable a)
joinNullable = unsafeCoerceColumn
