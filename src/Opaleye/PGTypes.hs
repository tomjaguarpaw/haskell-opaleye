{-# LANGUAGE EmptyDataDecls #-}

module Opaleye.PGTypes where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.Column as C

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Time as Time
import qualified Data.UUID as UUID

import           Data.Int (Int64)

data PGBool
data PGDate
data PGFloat4
data PGFloat8
data PGInt8
data PGInt4
data PGInt2
data PGNumeric
data PGText
data PGTime
data PGTimestamp
data PGTimestamptz
data PGUuid
data PGCitext
data PGArray a
data PGBytea

instance C.PGNum PGFloat8 where
  pgFromInteger = pgDouble . fromInteger

instance C.PGNum PGInt4 where
  pgFromInteger = pgInt4 . fromInteger

instance C.PGNum PGInt8 where
  pgFromInteger = pgInt8 . fromInteger

instance C.PGFractional PGFloat8 where
  pgFromRational = pgDouble . fromRational

literalColumn :: HPQ.Literal -> Column a
literalColumn = Column . HPQ.ConstExpr

pgString :: String -> Column PGText
pgString = literalColumn . HPQ.StringLit

pgLazyByteString :: LByteString.ByteString -> Column PGBytea
pgLazyByteString = literalColumn . HPQ.ByteStringLit . LByteString.toStrict

pgStrictByteString :: SByteString.ByteString -> Column PGBytea
pgStrictByteString = literalColumn . HPQ.ByteStringLit

pgStrictText :: SText.Text -> Column PGText
pgStrictText = literalColumn . HPQ.StringLit . SText.unpack

pgLazyText :: LText.Text -> Column PGText
pgLazyText = literalColumn . HPQ.StringLit . LText.unpack

pgInt4 :: Int -> Column PGInt4
pgInt4 = literalColumn . HPQ.IntegerLit . fromIntegral

pgInt8 :: Int64 -> Column PGInt8
pgInt8 = literalColumn . HPQ.IntegerLit . fromIntegral

pgDouble :: Double -> Column PGFloat8
pgDouble = literalColumn . HPQ.DoubleLit

pgBool :: Bool -> Column PGBool
pgBool = literalColumn . HPQ.BoolLit

pgUUID :: UUID.UUID -> Column PGUuid
pgUUID = C.unsafeCoerce . pgString . UUID.toString

-- Internal use only!
unsafePgFormatTime :: Time.FormatTime t => HPQ.Name -> String -> t -> Column c
unsafePgFormatTime typeName formatString = Column
                                     . HPQ.CastExpr typeName
                                     . HPQ.ConstExpr
                                     . HPQ.OtherLit
                                     . format
  where format = Time.formatTime Time.defaultTimeLocale formatString

pgDay :: Time.Day -> Column PGDate
pgDay = unsafePgFormatTime "date" "'%F'"

pgUTCTime :: Time.UTCTime -> Column PGTimestamptz
pgUTCTime = unsafePgFormatTime "timestamptz" "'%FT%TZ'"

pgLocalTime :: Time.LocalTime -> Column PGTimestamp
pgLocalTime = unsafePgFormatTime "timestamp" "'%FT%T'"

pgTimeOfDay :: Time.TimeOfDay -> Column PGTime
pgTimeOfDay = unsafePgFormatTime "time" "'%T'"

-- "We recommend not using the type time with time zone"
-- http://www.postgresql.org/docs/8.3/static/datatype-datetime.html


pgCiStrictText :: CI.CI SText.Text -> Column PGCitext
pgCiStrictText = literalColumn . HPQ.StringLit . SText.unpack . CI.original

pgCiLazyText :: CI.CI LText.Text -> Column PGCitext
pgCiLazyText = literalColumn . HPQ.StringLit . LText.unpack . CI.original

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)
