module Opaleye.Column (module Opaleye.Column,
                       Column,
                       Nullable,
                       unsafeCoerce)  where

import           Opaleye.Internal.Column (Column, Nullable, unsafeCoerce)
import qualified Opaleye.Internal.Column as C

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Prelude hiding (null)

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

maybeToNullable :: Maybe (Column a) -> Column (Nullable a)
maybeToNullable = maybe null toNullable

-- | Cast a column to any other type. This is safe for some conversions such as uuid to text.
unsafeCast :: String -> C.Column a -> Column b
unsafeCast = mapColumn . HPQ.CastExpr
  where
    mapColumn :: (HPQ.PrimExpr -> HPQ.PrimExpr) -> Column c -> Column a
    mapColumn primExpr c = C.Column (primExpr (C.unColumn c))
