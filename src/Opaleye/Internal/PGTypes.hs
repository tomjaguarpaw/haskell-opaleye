{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.PGTypes where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Data.Proxy (Proxy(..))
import qualified Data.Text as SText
import qualified Data.Text.Encoding as STextEncoding
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LTextEncoding
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Time.Compat as Time
import qualified Data.Time.Locale.Compat as Locale

unsafePgFormatTime :: Time.FormatTime t => HPQ.Name -> String -> t -> Column c
unsafePgFormatTime typeName formatString = castToType typeName . format
  where format = Time.formatTime Locale.defaultTimeLocale formatString

literalColumn :: forall a. IsSqlType a => HPQ.Literal -> Column a
literalColumn = Column . HPQ.CastExpr (showSqlType (Proxy :: Proxy a)) . HPQ.ConstExpr

castToType :: HPQ.Name -> String -> Column c
castToType typeName =
    Column . HPQ.CastExpr typeName . HPQ.ConstExpr . HPQ.OtherLit

strictDecodeUtf8 :: SByteString.ByteString -> String
strictDecodeUtf8 = SText.unpack . STextEncoding.decodeUtf8

lazyDecodeUtf8 :: LByteString.ByteString -> String
lazyDecodeUtf8 = LText.unpack . LTextEncoding.decodeUtf8

class IsSqlType sqlType where
  showSqlType :: proxy sqlType -> String

  {-# MINIMAL showSqlType #-}

instance IsSqlType a => IsSqlType (C.Nullable a) where
  showSqlType _ = showSqlType (Proxy :: Proxy a)
