{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.PGTypes where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.SqlType                      as SqlType

import           Data.Proxy (Proxy(..))
import qualified Data.Text as SText
import qualified Data.Text.Encoding as STextEncoding
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LTextEncoding
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Time as Time
import qualified Data.Time.Locale.Compat as Locale

unsafePgFormatTime :: Time.FormatTime t => HPQ.Name -> String -> t -> Column c
unsafePgFormatTime typeName formatString = castToType typeName . format
  where format = Time.formatTime Locale.defaultTimeLocale formatString

literalColumn :: forall a. IsSqlType a => HPQ.Literal -> Column a
literalColumn = Column . HPQ.CastExpr (toSqlType (Proxy :: Proxy a)) . HPQ.ConstExpr

castToType :: HPQ.Name -> String -> Column c
castToType typeName =
    Column
    . HPQ.CastExpr (SqlType.SqlBaseType typeName)
    . HPQ.ConstExpr
    . HPQ.OtherLit

strictDecodeUtf8 :: SByteString.ByteString -> String
strictDecodeUtf8 = SText.unpack . STextEncoding.decodeUtf8

lazyDecodeUtf8 :: LByteString.ByteString -> String
lazyDecodeUtf8 = LText.unpack . LTextEncoding.decodeUtf8

{-# DEPRECATED showPGType
    "Use 'toSqlType' instead. 'showPGType' will be removed \
    \in version 0.7." #-}

class IsSqlType sqlType where
  toSqlType :: proxy sqlType -> SqlType.SqlType
  toSqlType = SqlType.SqlBaseType . showSqlType

  showPGType :: proxy sqlType -> String
  showPGType  = showSqlType

  -- | Do not use @showSqlType@ Use 'toSqlType' instead. 'showSqlType'
  -- will be removed \ in version 0.7.
  showSqlType :: proxy sqlType -> String
  showSqlType = showPGType

  {-# MINIMAL showPGType | showSqlType | toSqlType #-}

instance IsSqlType a => IsSqlType (C.Nullable a) where
  showSqlType _ = showSqlType (Proxy :: Proxy a)
