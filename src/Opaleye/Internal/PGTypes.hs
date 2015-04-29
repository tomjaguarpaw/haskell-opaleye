module Opaleye.Internal.PGTypes where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

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

literalColumn :: HPQ.Literal -> Column a
literalColumn = Column . HPQ.ConstExpr

castToType :: HPQ.Name -> String -> Column c
castToType typeName =
    Column . HPQ.CastExpr typeName . HPQ.ConstExpr . HPQ.OtherLit

strictDecodeUtf8 :: SByteString.ByteString -> String
strictDecodeUtf8 = SText.unpack . STextEncoding.decodeUtf8

lazyDecodeUtf8 :: LByteString.ByteString -> String
lazyDecodeUtf8 = LText.unpack . LTextEncoding.decodeUtf8
