-- | Use "Opaleye.SqlTypes" instead.  Will be deprecated in version 0.7.

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.PGTypes (module Opaleye.PGTypes, IsSqlType(..)) where

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

pgTSVector :: String -> Column PGTSVector
pgTSVector = IPT.castToType "tsvector" . HSD.quote

pgTSQuery :: String -> Column PGTSQuery
pgTSQuery = IPT.castToType "tsquery" . HSD.quote

instance IsSqlType PGTSQuery where
  showSqlType _ = "tsquery"

-- * SQL datatypes

data PGTSQuery
data PGTSVector
