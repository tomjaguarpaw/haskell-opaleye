module Opaleye.SqlTypes (module Opaleye.SqlTypes,
                         P.IsSqlType,
                         P.IsRangeType) where

import qualified Opaleye.PGTypes as P
import           Opaleye.PGTypes (IsSqlType, IsRangeType)

import           Opaleye.Internal.Column (Column)
import qualified Opaleye.Internal.Column as C

import qualified Data.Aeson as Ae
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.CaseInsensitive as CI
import           Data.Int (Int64)
import           Data.Scientific as Sci
import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText
import qualified Data.Time as Time
import qualified Data.UUID as UUID

import qualified Database.PostgreSQL.Simple.Range as R

-- * Creating SQL values

sqlString :: String -> Column SqlText
sqlString = P.pgString

sqlLazyByteString :: LByteString.ByteString -> Column SqlBytea
sqlLazyByteString = P.pgLazyByteString

sqlStrictByteString :: SByteString.ByteString -> Column SqlBytea
sqlStrictByteString = P.pgStrictByteString

sqlStrictText :: SText.Text -> Column SqlText
sqlStrictText = P.pgStrictText

sqlLazyText :: LText.Text -> Column SqlText
sqlLazyText = P.pgLazyText

sqlNumeric :: Sci.Scientific -> Column SqlNumeric
sqlNumeric = P.pgNumeric

sqlInt4 :: Int -> Column SqlInt4
sqlInt4 = P.pgInt4

sqlInt8 :: Int64 -> Column SqlInt8
sqlInt8 = P.pgInt8

sqlDouble :: Double -> Column SqlFloat8
sqlDouble = P.pgDouble

sqlBool :: Bool -> Column SqlBool
sqlBool = P.pgBool

sqlUUID :: UUID.UUID -> Column SqlUuid
sqlUUID = P.pgUUID

sqlDay :: Time.Day -> Column SqlDate
sqlDay = P.pgDay

sqlUTCTime :: Time.UTCTime -> Column SqlTimestamptz
sqlUTCTime = P.pgUTCTime

sqlLocalTime :: Time.LocalTime -> Column SqlTimestamp
sqlLocalTime = P.pgLocalTime

sqlZonedTime :: Time.ZonedTime -> Column SqlTimestamptz
sqlZonedTime = P.pgZonedTime

sqlTimeOfDay :: Time.TimeOfDay -> Column SqlTime
sqlTimeOfDay = P.pgTimeOfDay

-- "We recommend not using the type time with time zone"
-- http://www.postgresql.org/docs/8.3/static/datatype-datetime.html


sqlCiStrictText :: CI.CI SText.Text -> Column SqlCitext
sqlCiStrictText = P.pgCiStrictText

sqlCiLazyText :: CI.CI LText.Text -> Column SqlCitext
sqlCiLazyText = P.pgCiLazyText

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

-- The json data type was introduced in PostgreSQL version 9.2
-- JSON values must be SQL string quoted
sqlJSON :: String -> Column SqlJson
sqlJSON = P.pgJSON

sqlStrictJSON :: SByteString.ByteString -> Column SqlJson
sqlStrictJSON = P.pgStrictJSON

sqlLazyJSON :: LByteString.ByteString -> Column SqlJson
sqlLazyJSON = P.pgLazyJSON

sqlValueJSON :: Ae.ToJSON a => a -> Column SqlJson
sqlValueJSON = P.pgValueJSON

-- The jsonb data type was introduced in PostgreSQL version 9.4
-- JSONB values must be SQL string quoted
--
-- TODO: We need to add literal JSON and JSONB types so we can say
-- `castToTypeTyped JSONB` rather than `castToType "jsonb"`.
sqlJSONB :: String -> Column SqlJsonb
sqlJSONB = P.pgJSONB

sqlStrictJSONB :: SByteString.ByteString -> Column SqlJsonb
sqlStrictJSONB = P.pgStrictJSONB

sqlLazyJSONB :: LByteString.ByteString -> Column SqlJsonb
sqlLazyJSONB = P.pgLazyJSONB

sqlValueJSONB :: Ae.ToJSON a => a -> Column SqlJsonb
sqlValueJSONB = P.pgValueJSONB

sqlArray :: IsSqlType b => (a -> C.Column b) -> [a] -> C.Column (SqlArray b)
sqlArray = P.pgArray

sqlRange :: IsRangeType b
         => (a -> C.Column b)
         -> R.RangeBound a
         -> R.RangeBound a
         -> C.Column (SqlRange b)
sqlRange = P.pgRange

-- * SQL datatypes

type SqlBool = P.PGBool
type SqlDate = P.PGDate
type SqlFloat4 = P.PGFloat4
type SqlFloat8 = P.PGFloat8
type SqlInt8 = P.PGInt8
type SqlInt4 = P.PGInt4
type SqlInt2 = P.PGInt2
type SqlNumeric = P.PGNumeric
type SqlText = P.PGText
type SqlTime = P.PGTime
type SqlTimestamp = P.PGTimestamp
type SqlTimestamptz = P.PGTimestamptz
type SqlUuid = P.PGUuid
type SqlCitext = P.PGCitext
type SqlArray = P.PGArray
type SqlBytea = P.PGBytea
type SqlJson = P.PGJson
type SqlJsonb = P.PGJsonb
type SqlRange = P.PGRange
