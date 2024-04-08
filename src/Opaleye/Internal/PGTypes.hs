{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.PGTypes where

import           Opaleye.Internal.Column (Field, Field_(Column))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Data.Proxy (Proxy(..))
import qualified Data.Text as SText
import qualified Data.Text.Encoding as STextEncoding
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LTextEncoding
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Time.Format.ISO8601.Compat as Time
import Text.PrettyPrint.HughesPJ ((<>), doubleQuotes, render, text)
import Prelude hiding ((<>))

unsafePgFormatTime :: Time.ISO8601 t => HPQ.Name -> t -> Field c
unsafePgFormatTime typeName = castToType typeName . format
    where
      format  = quote . Time.iso8601Show
      quote s = "'" ++ s ++ "'"

literalColumn :: forall a. IsSqlType a => HPQ.Literal -> Field a
literalColumn = Column . HPQ.CastExpr (showSqlType (Proxy :: Proxy a)) . HPQ.ConstExpr

castToType :: HPQ.Name -> String -> Field_ n c
castToType typeName =
    Column . HPQ.CastExpr typeName . HPQ.ConstExpr . HPQ.OtherLit

strictDecodeUtf8 :: SByteString.ByteString -> String
strictDecodeUtf8 = SText.unpack . STextEncoding.decodeUtf8

lazyDecodeUtf8 :: LByteString.ByteString -> String
lazyDecodeUtf8 = LText.unpack . LTextEncoding.decodeUtf8

class IsSqlType sqlType where
  showSqlType :: proxy sqlType -> String
  showSqlType sqlType = render (doubleQuotes (text schema) <> text "." <> doubleQuotes (text type_))
                    where (schema, type_) = showSqlTypeWithSchema sqlType
  showSqlTypeWithSchema :: proxy sqlType -> (String, String)

  {-# MINIMAL showSqlType | showSqlTypeWithSchema #-}
