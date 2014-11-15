module Opaleye.Column (module Opaleye.Column,
                       Column,
                       Nullable,
                       unsafeCoerce)  where

import           Opaleye.Internal.Column (Column, Nullable, unsafeCoerce)
import qualified Opaleye.Internal.Column as C

import qualified Database.HaskellDB.PrimQuery as HPQ

-- | A NULL of any type
null :: Column (Nullable a)
null = unsafeCoerce (C.Column (HPQ.ConstExpr HPQ.NullLit))

isNull :: Column (Nullable a) -> Column Bool
isNull = C.unOp HPQ.OpIsNull

-- | The Opaleye equivalent of the maybe function
matchNullable :: Column b -> (Column a -> Column b) -> Column (Nullable a)
              -> Column b
matchNullable replacement f x = C.ifThenElse (isNull x) replacement
                                             (f (unsafeCoerce x))

-- | The Opaleye equivalent of the fromMaybe function
fromNullable :: Column a -> Column (Nullable a) -> Column a
fromNullable = flip matchNullable id

toNullable :: Column a -> Column (Nullable a)
toNullable = unsafeCoerce
