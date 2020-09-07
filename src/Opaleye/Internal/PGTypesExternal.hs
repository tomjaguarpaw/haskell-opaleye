{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Opaleye.Internal.PGTypesExternal
  (module Opaleye.Internal.PGTypesExternal, IsSqlType(..)) where

import           Opaleye.Internal.Column (Column)
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
import qualified Data.Time as Time
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

instance C.SqlString SqlCitext where
  sqlFromString = pgCiLazyText . CI.mk . LText.pack

-- * Creating SQL values

pgString :: String -> Column PGText
pgString = IPT.literalColumn . HPQ.StringLit

pgLazyByteString :: LByteString.ByteString -> Column PGBytea
pgLazyByteString = IPT.literalColumn . HPQ.ByteStringLit . LByteString.toStrict

pgStrictByteString :: SByteString.ByteString -> Column PGBytea
pgStrictByteString = IPT.literalColumn . HPQ.ByteStringLit

pgStrictText :: SText.Text -> Column PGText
pgStrictText = IPT.literalColumn . HPQ.StringLit . SText.unpack

pgLazyText :: LText.Text -> Column PGText
pgLazyText = IPT.literalColumn . HPQ.StringLit . LText.unpack

pgNumeric :: Sci.Scientific -> Column PGNumeric
pgNumeric = IPT.literalColumn . HPQ.NumericLit

pgInt4 :: Int -> Column PGInt4
pgInt4 = IPT.literalColumn . HPQ.IntegerLit . fromIntegral

pgInt8 :: Int64 -> Column PGInt8
pgInt8 = IPT.literalColumn . HPQ.IntegerLit . fromIntegral

pgDouble :: Double -> Column PGFloat8
pgDouble = IPT.literalColumn . HPQ.DoubleLit

pgBool :: Bool -> Column PGBool
pgBool = IPT.literalColumn . HPQ.BoolLit

pgUUID :: UUID.UUID -> Column PGUuid
pgUUID = IPT.literalColumn . HPQ.StringLit . UUID.toString

pgDay :: Time.Day -> Column PGDate
pgDay = IPT.unsafePgFormatTime "date" "'%F'"

pgUTCTime :: Time.UTCTime -> Column PGTimestamptz
pgUTCTime = IPT.unsafePgFormatTime "timestamptz" "'%FT%T%QZ'"

pgLocalTime :: Time.LocalTime -> Column PGTimestamp
pgLocalTime = IPT.unsafePgFormatTime "timestamp" "'%FT%T%Q'"

pgZonedTime :: Time.ZonedTime -> Column PGTimestamptz
pgZonedTime = IPT.unsafePgFormatTime "timestamptz" "'%FT%T%Q%z'"

pgTimeOfDay :: Time.TimeOfDay -> Column PGTime
pgTimeOfDay = IPT.unsafePgFormatTime "time" "'%T%Q'"

-- "We recommend not using the type time with time zone"
-- http://www.postgresql.org/docs/8.3/static/datatype-datetime.html


pgCiStrictText :: CI.CI SText.Text -> Column PGCitext
pgCiStrictText = IPT.literalColumn . HPQ.StringLit . SText.unpack . CI.original

pgCiLazyText :: CI.CI LText.Text -> Column PGCitext
pgCiLazyText = IPT.literalColumn . HPQ.StringLit . LText.unpack . CI.original

-- No CI String instance since postgresql-simple doesn't define
-- FromField (CI String)

-- The json data type was introduced in PostgreSQL version 9.2
-- JSON values must be SQL string quoted
pgJSON :: String -> Column PGJson
pgJSON = IPT.castToType "json" . HSD.quote

pgStrictJSON :: SByteString.ByteString -> Column PGJson
pgStrictJSON = pgJSON . IPT.strictDecodeUtf8

pgLazyJSON :: LByteString.ByteString -> Column PGJson
pgLazyJSON = pgJSON . IPT.lazyDecodeUtf8

pgValueJSON :: Ae.ToJSON a => a -> Column PGJson
pgValueJSON = pgLazyJSON . Ae.encode

-- The jsonb data type was introduced in PostgreSQL version 9.4
-- JSONB values must be SQL string quoted
--
-- TODO: We need to add literal JSON and JSONB types so we can say
-- `castToTypeTyped JSONB` rather than `castToType "jsonb"`.
pgJSONB :: String -> Column PGJsonb
pgJSONB = IPT.castToType "jsonb" . HSD.quote

pgStrictJSONB :: SByteString.ByteString -> Column PGJsonb
pgStrictJSONB = pgJSONB . IPT.strictDecodeUtf8

pgLazyJSONB :: LByteString.ByteString -> Column PGJsonb
pgLazyJSONB = pgJSONB . IPT.lazyDecodeUtf8

pgValueJSONB :: Ae.ToJSON a => a -> Column PGJsonb
pgValueJSONB = pgLazyJSONB . Ae.encode

pgArray :: forall a b. IsSqlType b
        => (a -> C.Column b) -> [a] -> C.Column (PGArray b)
pgArray pgEl xs = C.unsafeCast arrayTy $
  C.Column (HPQ.ArrayExpr (map oneEl xs))
  where
    oneEl = C.unColumn . pgEl
    arrayTy = showSqlType ([] :: [PGArray b])

pgRange :: forall a b. IsRangeType b
        => (a -> C.Column b) -> R.RangeBound a -> R.RangeBound a
        -> C.Column (PGRange b)
pgRange pgEl start end =
  C.Column (HPQ.RangeExpr (showRangeType ([] :: [b])) (oneEl start) (oneEl end))
  where oneEl (R.Inclusive a) = HPQ.Inclusive . C.unColumn $ pgEl a
        oneEl (R.Exclusive a) = HPQ.Exclusive . C.unColumn $ pgEl a
        oneEl R.NegInfinity   = HPQ.NegInfinity
        oneEl R.PosInfinity   = HPQ.PosInfinity

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
instance IsSqlType SqlNumeric where
  showSqlType _ = "numeric"
instance IsSqlType SqlText where
  showSqlType _ = "text"
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
instance IsSqlType a => IsSqlType (SqlArray a) where
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

-- * SQL datatypes

data SqlBool
data SqlDate
data SqlFloat4
data SqlFloat8
data SqlInt8
data SqlInt4
data SqlInt2
data SqlNumeric
data SqlText
data SqlTime
data SqlTimestamp
data SqlTimestamptz
data SqlUuid
data SqlCitext
data SqlArray a
data SqlBytea
data SqlJson
data SqlJsonb
data SqlRange a

type PGBool = SqlBool
type PGDate = SqlDate
type PGFloat4 = SqlFloat4
type PGFloat8 = SqlFloat8
type PGInt8 = SqlInt8
type PGInt4 = SqlInt4
type PGInt2 = SqlInt2
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
