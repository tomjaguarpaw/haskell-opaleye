{-# LANGUAGE EmptyDataDecls #-}

module Opaleye.SQLite.SqlTypes (module Opaleye.SQLite.SqlTypes) where

import           Opaleye.SQLite.Internal.Column (Column)
import qualified Opaleye.SQLite.PGTypes as PT
import qualified Opaleye.SQLite.Internal.PGTypes as IPT

import qualified Opaleye.SQLite.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText
import qualified Data.Time as Time

-- These probably don't correspond very well to SQLite types yet.
-- Work in progress.
type SqlBool   = PT.PGBool
type SqlDate   = PT.PGDate
type SqlReal   = PT.PGFloat8
type SqlText   = PT.PGText
type SqlInt    = PT.PGInt4

sqlString :: String -> Column SqlText
sqlString = PT.pgString

sqlStrictText :: SText.Text -> Column SqlText
sqlStrictText = PT.pgStrictText

sqlLazyText :: LText.Text -> Column SqlText
sqlLazyText = PT.pgLazyText

sqlInt :: Int -> Column SqlInt
sqlInt = PT.pgInt4

sqlReal :: Double -> Column SqlReal
sqlReal = PT.pgDouble

sqlBool :: Bool -> Column SqlBool
sqlBool = PT.pgBool
