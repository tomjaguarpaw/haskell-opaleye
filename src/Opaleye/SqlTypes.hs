-- | SQL types and functions to create 'Opaleye.Field.Field_'s of
-- those types.  To create fields you may find it more convenient to use
-- "Opaleye.ToFields" instead.

module Opaleye.SqlTypes (
  -- * Numeric
  -- ** Creating values
  sqlInt4,
  sqlDouble,
  sqlInt8,
  sqlNumeric,
  -- ** Types
  SqlInt4,
  SqlFloat8,
  SqlNumeric,
  SqlInt8,
  SqlInt2,
  SqlFloat4,
  -- ** Type classes
  IC.SqlNum,
  IC.SqlIntegral,
  IC.SqlFractional,
  -- * Date and time
  -- ** Creating values
  sqlDay,
  sqlUTCTime,
  sqlLocalTime,
  sqlZonedTime,
  sqlTimeOfDay,
  P.sqlInterval,
  -- ** Types
  SqlDate,
  SqlTime,
  SqlTimestamp,
  SqlTimestamptz,
  SqlInterval,
  -- * JSON
  -- ** Creating values
  sqlJSON,
  sqlStrictJSON,
  sqlLazyJSON,
  sqlValueJSON,
  -- ** Types
  SqlJson,
  -- * JSONB
  -- ** Creating values
  sqlJSONB,
  sqlStrictJSONB,
  sqlLazyJSONB,
  sqlValueJSONB,
  -- ** Types
  SqlJsonb,
  -- * Text
  -- ** Creating values
  sqlString,
  sqlStrictText,
  sqlLazyText,
  P.sqlStringVarcharN,
  P.sqlStrictTextVarcharN,
  P.sqlLazyTextVarcharN,
  sqlCiStrictText,
  sqlCiLazyText,
  -- ** Types
  SqlText,
  SqlVarcharN,
  SqlCitext,
  -- ** Type classes
  IC.SqlString,
  -- * Array
  -- ** Creating values
  sqlArray,
  -- ** Types
  SqlArray,
  SqlArray_,
  -- * Range
  -- ** Creating values
  sqlRange,
  -- ** Types
  SqlRange,
  P.IsRangeType,
  -- * Other
  -- ** Creating values
  sqlBool,
  sqlUUID,
  sqlLazyByteString,
  sqlStrictByteString,
  -- ** Types
  SqlBool,
  SqlUuid,
  SqlBytea,
  -- * @IsSqlType@
  P.IsSqlType(P.showSqlType),
  IPT.sqlTypeWithSchema,
  ) where

import qualified Opaleye.Field   as F
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.PGTypes as IPT
import qualified Opaleye.Internal.PGTypesExternal as P
import           Opaleye.Internal.PGTypesExternal (IsSqlType, IsRangeType)
import           Opaleye.Internal.PGTypesExternal (SqlBool,
                                                   SqlDate,
                                                   SqlFloat4,
                                                   SqlFloat8,
                                                   SqlInt8,
                                                   SqlInt4,
                                                   SqlInt2,
                                                   SqlNumeric,
                                                   SqlText,
                                                   SqlVarcharN,
                                                   SqlTime,
                                                   SqlTimestamp,
                                                   SqlTimestamptz,
                                                   SqlInterval,
                                                   SqlUuid,
                                                   SqlCitext,
                                                   SqlArray,
                                                   SqlArray_,
                                                   SqlBytea,
                                                   SqlJson,
                                                   SqlJsonb,
                                                   SqlRange)

import qualified Data.Aeson as Ae
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.CaseInsensitive as CI
import           Data.Int (Int64)
import           Data.Scientific as Sci
import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText
import qualified Data.Time.Compat as Time
import qualified Data.UUID as UUID

import qualified Database.PostgreSQL.Simple.Range as R

-- * Creating SQL values

sqlString :: String -> F.Field SqlText
sqlString = P.pgString

sqlLazyByteString :: LByteString.ByteString -> F.Field SqlBytea
sqlLazyByteString = P.pgLazyByteString

sqlStrictByteString :: SByteString.ByteString -> F.Field SqlBytea
sqlStrictByteString = P.pgStrictByteString

sqlStrictText :: SText.Text -> F.Field SqlText
sqlStrictText = P.pgStrictText

sqlLazyText :: LText.Text -> F.Field SqlText
sqlLazyText = P.pgLazyText

sqlNumeric :: Sci.Scientific -> F.Field SqlNumeric
sqlNumeric = P.pgNumeric

sqlInt4 :: Int -> F.Field SqlInt4
sqlInt4 = P.pgInt4

sqlInt8 :: Int64 -> F.Field SqlInt8
sqlInt8 = P.pgInt8

sqlDouble :: Double -> F.Field SqlFloat8
sqlDouble = P.pgDouble

sqlBool :: Bool -> F.Field SqlBool
sqlBool = P.pgBool

sqlUUID :: UUID.UUID -> F.Field SqlUuid
sqlUUID = P.pgUUID

sqlDay :: Time.Day -> F.Field SqlDate
sqlDay = P.pgDay

sqlUTCTime :: Time.UTCTime -> F.Field SqlTimestamptz
sqlUTCTime = P.pgUTCTime

sqlLocalTime :: Time.LocalTime -> F.Field SqlTimestamp
sqlLocalTime = P.pgLocalTime

sqlZonedTime :: Time.ZonedTime -> F.Field SqlTimestamptz
sqlZonedTime = P.pgZonedTime

sqlTimeOfDay :: Time.TimeOfDay -> F.Field SqlTime
sqlTimeOfDay = P.pgTimeOfDay

-- "We recommend not using the type time with time zone"
-- http://www.postgresql.org/docs/8.3/static/datatype-datetime.html

sqlCiStrictText :: CI.CI SText.Text -> F.Field SqlCitext
sqlCiStrictText = P.pgCiStrictText

sqlCiLazyText :: CI.CI LText.Text -> F.Field SqlCitext
sqlCiLazyText = P.pgCiLazyText

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

-- The json data type was introduced in PostgreSQL version 9.2
-- JSON values must be SQL string quoted
sqlJSON :: String -> F.Field SqlJson
sqlJSON = P.pgJSON

sqlStrictJSON :: SByteString.ByteString -> F.Field SqlJson
sqlStrictJSON = P.pgStrictJSON

sqlLazyJSON :: LByteString.ByteString -> F.Field SqlJson
sqlLazyJSON = P.pgLazyJSON

sqlValueJSON :: Ae.ToJSON a => a -> F.Field SqlJson
sqlValueJSON = P.pgValueJSON

-- The jsonb data type was introduced in PostgreSQL version 9.4
-- JSONB values must be SQL string quoted
--
-- TODO: We need to add literal JSON and JSONB types so we can say
-- `castToTypeTyped JSONB` rather than `castToType "jsonb"`.
sqlJSONB :: String -> F.Field SqlJsonb
sqlJSONB = P.pgJSONB

sqlStrictJSONB :: SByteString.ByteString -> F.Field SqlJsonb
sqlStrictJSONB = P.pgStrictJSONB

sqlLazyJSONB :: LByteString.ByteString -> F.Field SqlJsonb
sqlLazyJSONB = P.pgLazyJSONB

sqlValueJSONB :: Ae.ToJSON a => a -> F.Field SqlJsonb
sqlValueJSONB = P.pgValueJSONB

sqlArray :: IsSqlType b => (a -> F.Field_ n b) -> [a] -> F.Field (SqlArray_ n b)
sqlArray = P.pgArray

sqlRange :: IsRangeType b
         => (a -> F.Field b)
         -> R.RangeBound a
         -> R.RangeBound a
         -> F.Field (SqlRange b)
sqlRange = P.pgRange
