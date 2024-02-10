{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Opaleye.Internal.PGTypesExternal
  (module Opaleye.Internal.PGTypesExternal, IsSqlType(..)) where

import           Opaleye.Internal.Column (Field_, Field)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.PGTypes as IPT
import           Opaleye.Internal.PGTypes (IsSqlType(..))

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.HaskellDB.Sql.Default as HSD

import qualified Data.CaseInsensitive as CI
import qualified Data.Aeson as Ae
import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import           Data.Scientific as Sci
import qualified Data.Time.Compat as Time
import qualified Data.UUID as UUID

import           Data.Int (Int64)

import qualified Database.PostgreSQL.Simple.Range as R

instance C.SqlNum SqlFloat8 where
  sqlFromInteger = pgDouble . fromInteger

instance C.SqlNum SqlInt4 where
  sqlFromInteger = pgInt4 . fromInteger

instance C.SqlNum SqlInt8 where
  sqlFromInteger = pgInt8 . fromInteger

instance C.SqlNum SqlNumeric where
  sqlFromInteger = pgNumeric . fromInteger

instance C.SqlFractional SqlFloat8 where
  sqlFromRational = pgDouble . fromRational

instance C.SqlIntegral SqlInt2
instance C.SqlIntegral SqlNumeric
instance C.SqlIntegral SqlInt4
instance C.SqlIntegral SqlInt8

instance C.SqlString SqlText where
  sqlFromString = pgString

instance C.SqlString SqlVarcharN where
  sqlFromString = sqlStringVarcharN

instance C.SqlString SqlCitext where
  sqlFromString = pgCiLazyText . CI.mk . LText.pack

-- * Creating SQL values

pgString :: String -> Field PGText
pgString = IPT.literalColumn . HPQ.StringLit

pgLazyByteString :: LByteString.ByteString -> Field PGBytea
pgLazyByteString = IPT.literalColumn . HPQ.ByteStringLit . LByteString.toStrict

pgStrictByteString :: SByteString.ByteString -> Field PGBytea
pgStrictByteString = IPT.literalColumn . HPQ.ByteStringLit

pgStrictText :: SText.Text -> Field PGText
pgStrictText = IPT.literalColumn . HPQ.StringLit . SText.unpack

pgLazyText :: LText.Text -> Field PGText
pgLazyText = IPT.literalColumn . HPQ.StringLit . LText.unpack

sqlStringVarcharN :: String -> Field SqlVarcharN
sqlStringVarcharN = IPT.literalColumn . HPQ.StringLit

sqlStrictTextVarcharN :: SText.Text -> Field SqlVarcharN
sqlStrictTextVarcharN = IPT.literalColumn . HPQ.StringLit . SText.unpack

sqlLazyTextVarcharN :: LText.Text -> Field SqlVarcharN
sqlLazyTextVarcharN = IPT.literalColumn . HPQ.StringLit . LText.unpack

pgNumeric :: Sci.Scientific -> Field PGNumeric
pgNumeric = IPT.literalColumn . HPQ.NumericLit

pgInt4 :: Int -> Field PGInt4
pgInt4 = IPT.literalColumn . HPQ.IntegerLit . fromIntegral

pgInt8 :: Int64 -> Field PGInt8
pgInt8 = IPT.literalColumn . HPQ.IntegerLit . fromIntegral

pgDouble :: Double -> Field PGFloat8
pgDouble = IPT.literalColumn . HPQ.DoubleLit

pgBool :: Bool -> Field PGBool
pgBool = IPT.literalColumn . HPQ.BoolLit

pgUUID :: UUID.UUID -> Field PGUuid
pgUUID = IPT.literalColumn . HPQ.StringLit . UUID.toString

pgDay :: Time.Day -> Field PGDate
pgDay = IPT.unsafePgFormatTime "date"

pgUTCTime :: Time.UTCTime -> Field PGTimestamptz
pgUTCTime = IPT.unsafePgFormatTime "timestamptz"

pgLocalTime :: Time.LocalTime -> Field PGTimestamp
pgLocalTime = IPT.unsafePgFormatTime "timestamp"

pgZonedTime :: Time.ZonedTime -> Field PGTimestamptz
pgZonedTime = IPT.unsafePgFormatTime "timestamptz"

pgTimeOfDay :: Time.TimeOfDay -> Field PGTime
pgTimeOfDay = IPT.unsafePgFormatTime "time"

-- "We recommend not using the type time with time zone"
-- http://www.postgresql.org/docs/8.3/static/datatype-datetime.html

sqlInterval :: Time.CalendarDiffTime -> Field PGInterval
sqlInterval = IPT.unsafePgFormatTime "interval"

pgCiStrictText :: CI.CI SText.Text -> Field PGCitext
pgCiStrictText = IPT.literalColumn . HPQ.StringLit . SText.unpack . CI.original

pgCiLazyText :: CI.CI LText.Text -> Field PGCitext
pgCiLazyText = IPT.literalColumn . HPQ.StringLit . LText.unpack . CI.original

-- No CI String instance since postgresql-simple doesn't define
-- FromField (CI String)

-- The json data type was introduced in PostgreSQL version 9.2
-- JSON values must be SQL string quoted
pgJSON :: String -> Field PGJson
pgJSON = IPT.castToType "json" . HSD.quote

pgStrictJSON :: SByteString.ByteString -> Field PGJson
pgStrictJSON = pgJSON . IPT.strictDecodeUtf8

pgLazyJSON :: LByteString.ByteString -> Field PGJson
pgLazyJSON = pgJSON . IPT.lazyDecodeUtf8

pgValueJSON :: Ae.ToJSON a => a -> Field PGJson
pgValueJSON = pgLazyJSON . Ae.encode

-- The jsonb data type was introduced in PostgreSQL version 9.4
-- JSONB values must be SQL string quoted
--
-- TODO: We need to add literal JSON and JSONB types so we can say
-- `castToTypeTyped JSONB` rather than `castToType "jsonb"`.
pgJSONB :: String -> Field PGJsonb
pgJSONB = IPT.castToType "jsonb" . HSD.quote

pgStrictJSONB :: SByteString.ByteString -> Field PGJsonb
pgStrictJSONB = pgJSONB . IPT.strictDecodeUtf8

pgLazyJSONB :: LByteString.ByteString -> Field PGJsonb
pgLazyJSONB = pgJSONB . IPT.lazyDecodeUtf8

pgValueJSONB :: Ae.ToJSON a => a -> Field PGJsonb
pgValueJSONB = pgLazyJSONB . Ae.encode

pgArray :: forall a b n. IsSqlType b
        => (a -> Field_ n b) -> [a] -> Field (SqlArray_ n b)
pgArray pgEl xs = C.unsafeCast arrayTy $
  C.Column (HPQ.ArrayExpr (map oneEl xs))
  where
    oneEl = C.unColumn . pgEl
    arrayTy = showSqlType ([] :: [SqlArray_ n b])

pgRange :: forall a b n n'. IsRangeType b
        => (a -> Field_ n b) -> R.RangeBound a -> R.RangeBound a
        -> Field_ n' (SqlRange b)
pgRange pgEl start end =
  C.Column (HPQ.RangeExpr (showRangeType ([] :: [b])) (oneEl start) (oneEl end))
  where oneEl (R.Inclusive a) = HPQ.Inclusive . C.unColumn $ pgEl a
        oneEl (R.Exclusive a) = HPQ.Exclusive . C.unColumn $ pgEl a
        oneEl R.NegInfinity   = HPQ.NegInfinity
        oneEl R.PosInfinity   = HPQ.PosInfinity

-- Full Text Search

pgTSVector :: Field SqlText -> Field SqlTSVector
pgTSVector (C.Column e) = C.Column (HPQ.FunExpr "tsvector" [e])

pgTSQuery :: Field SqlText -> Field SqlTSQuery
pgTSQuery (C.Column e) = C.Column (HPQ.FunExpr "tsquery" [e])


instance IsSqlType SqlBool where
  showSqlType _ = "boolean"
instance IsSqlType SqlDate where
  showSqlType _ = "date"
instance IsSqlType SqlFloat4 where
  showSqlType _ = "real"
instance IsSqlType SqlFloat8 where
  showSqlType _ = "double precision"
instance IsSqlType SqlInt8 where
  showSqlType _ = "bigint"
instance IsSqlType SqlInt4 where
  showSqlType _ = "integer"
instance IsSqlType SqlInt2 where
  showSqlType _ = "smallint"
instance IsSqlType SqlInterval where
  showSqlType _ = "interval"
instance IsSqlType SqlNumeric where
  showSqlType _ = "numeric"
instance IsSqlType SqlText where
  showSqlType _ = "text"
instance IsSqlType SqlVarcharN where
  showSqlType _ = "varchar"
instance IsSqlType SqlTime where
  showSqlType _ = "time"
instance IsSqlType SqlTimestamp where
  showSqlType _ = "timestamp"
instance IsSqlType SqlTimestamptz where
  showSqlType _ = "timestamp with time zone"
instance IsSqlType SqlUuid where
  showSqlType _ = "uuid"
instance IsSqlType SqlCitext where
  showSqlType _ =  "citext"
instance IsSqlType SqlBytea where
  showSqlType _ = "bytea"
instance IsSqlType a => IsSqlType (SqlArray_ n a) where
  showSqlType _ = showSqlType ([] :: [a]) ++ "[]"
instance IsSqlType SqlJson where
  showSqlType _ = "json"
instance IsSqlType SqlJsonb where
  showSqlType _ = "jsonb"
instance IsRangeType a => IsSqlType (SqlRange a) where
  showSqlType _ = showRangeType ([] :: [a])

class IsSqlType pgType => IsRangeType pgType where
  showRangeType :: proxy pgType -> String

instance IsRangeType SqlInt4 where
  showRangeType _ = "int4range"

instance IsRangeType SqlInt8 where
  showRangeType _ = "int8range"

instance IsRangeType SqlNumeric where
  showRangeType _ = "numrange"

instance IsRangeType SqlTimestamp where
  showRangeType _ = "tsrange"

instance IsRangeType SqlTimestamptz where
  showRangeType _ = "tstzrange"

instance IsRangeType SqlDate where
  showRangeType _ = "daterange"

instance IsSqlType SqlTSQuery where
  showSqlType _ = "tsquery"

-- * SQL datatypes

data SqlBool
data SqlDate
data SqlFloat4
data SqlFloat8
data SqlInt8
data SqlInt4
data SqlInt2
-- | Requires you to configure @intervalstyle@ as @iso_8601@.
--
-- You can configure @intervalstyle@ on every connection with a @SET@ command,
-- but for better performance you may want to configure it permanently in the
-- file found with @SHOW config_file;@.
data SqlInterval
data SqlNumeric
data SqlText
-- | @VARCHAR(n)@ for any @n@.  Opaleye does not do anything to check
-- that the @n@ you choose is correctly adhered to!
data SqlVarcharN
data SqlTime
data SqlTimestamp
-- | Be careful if you use Haskell's `Time.ZonedTime` with
-- @SqlTimestamptz@. A Postgres @timestamptz@ does not actually
-- contain any time zone.  It is just a UTC time that is automatically
-- converted to or from local time on certain occasions, according to
-- the [timezone setting of the
-- server](https://www.postgresql.org/docs/9.1/runtime-config-client.html#GUC-TIMEZONE).
-- Therefore, although when you roundtrip an input 'Time.ZonedTime' to
-- obtain an output 'Time.ZonedTime' they each refer to the same
-- instant in time, the time zone attached to the output will not
-- necessarily the same as the time zone attached to the input.
data SqlTimestamptz
data SqlUuid
data SqlCitext
data SqlArray_ (n :: C.Nullability) a
type SqlArray = SqlArray_ C.NonNullable
data SqlBytea
data SqlJson
data SqlJsonb
data SqlRange a
data SqlTSQuery
data SqlTSVector

type PGBool = SqlBool
type PGDate = SqlDate
type PGFloat4 = SqlFloat4
type PGFloat8 = SqlFloat8
type PGInt8 = SqlInt8
type PGInt4 = SqlInt4
type PGInt2 = SqlInt2
type PGInterval = SqlInterval
type PGNumeric = SqlNumeric
type PGText = SqlText
type PGTime = SqlTime
type PGTimestamp = SqlTimestamp
type PGTimestamptz = SqlTimestamptz
type PGUuid = SqlUuid
type PGCitext = SqlCitext
type PGArray = SqlArray
type PGBytea = SqlBytea
type PGJson = SqlJson
type PGJsonb = SqlJsonb
type PGRange = SqlRange
