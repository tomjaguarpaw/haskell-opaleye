{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Constant where

import           Opaleye.Column                  (Column)
import qualified Opaleye.Column                  as C
import qualified Opaleye.SqlTypes                 as T

import qualified Data.Aeson                      as Ae
import qualified Data.CaseInsensitive            as CI
import qualified Data.Int                        as Int
import qualified Data.Text                       as ST
import qualified Data.Text.Lazy                  as LT
import qualified Data.ByteString                 as SBS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Scientific                 as Sci
import qualified Data.Time                       as Time
import qualified Data.UUID                       as UUID

import qualified Data.Profunctor.Product         as PP
import           Data.Profunctor.Product         (empty, (***!), (+++!))
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor                 as P

import           Control.Applicative (Applicative, pure, (<*>))
import           Data.Functor                    ((<$>))

import qualified Database.PostgreSQL.Simple.Range as R

-- | 'toFields' provides a convenient typeclass wrapper around the
-- 'Column' creation functions in "Opaleye.SqlTypes".  Besides
-- convenience it doesn't provide any additional functionality.
--
-- It can be used with functions like 'Opaleye.Manipulation.runInsert'
-- to insert custom Haskell types into the database.
-- The following is an example of a function for inserting custom types.
--
-- @
--   customInsert
--      :: ( 'D.Default' 'Constant' haskells columns )
--      => Connection
--      -> 'Opaleye.Table' columns columns'
--      -> haskells
--      -> IO Int64
--   customInsert conn table haskells = 'Opaleye.Manipulation.runInsert' conn table $ 'toFields' haskells
-- @
--
-- In order to use this function with your custom types, you need to define an
-- instance of 'D.Default' 'Constant' for your custom types.
toFields :: D.Default Constant haskells columns
         => haskells -> columns
toFields = constantExplicit D.def

constant :: D.Default Constant haskells columns
         => haskells -> columns
constant = constantExplicit D.def

newtype Constant haskells columns =
  Constant { constantExplicit :: haskells -> columns }

type ToFields = Constant

instance D.Default Constant haskell (Column sql)
         => D.Default Constant (Maybe haskell) (Column (C.Nullable sql)) where
  def = Constant (C.maybeToNullable . fmap f)
    where Constant f = D.def

instance D.Default Constant String (Column T.SqlText) where
  def = Constant T.sqlString

instance D.Default Constant LBS.ByteString (Column T.SqlBytea) where
  def = Constant T.sqlLazyByteString

instance D.Default Constant SBS.ByteString (Column T.SqlBytea) where
  def = Constant T.sqlStrictByteString

instance D.Default Constant ST.Text (Column T.SqlText) where
  def = Constant T.sqlStrictText

instance D.Default Constant LT.Text (Column T.SqlText) where
  def = Constant T.sqlLazyText

instance D.Default Constant Sci.Scientific (Column T.SqlNumeric) where
  def = Constant T.sqlNumeric

instance D.Default Constant Int (Column T.SqlInt4) where
  def = Constant T.sqlInt4

instance D.Default Constant Int.Int32 (Column T.SqlInt4) where
  def = Constant $ T.sqlInt4 . fromIntegral

instance D.Default Constant Int.Int64 (Column T.SqlInt8) where
  def = Constant T.sqlInt8

instance D.Default Constant Double (Column T.SqlFloat8) where
  def = Constant T.sqlDouble

instance D.Default Constant Bool (Column T.SqlBool) where
  def = Constant T.sqlBool

instance D.Default Constant UUID.UUID (Column T.SqlUuid) where
  def = Constant T.sqlUUID

instance D.Default Constant Time.Day (Column T.SqlDate) where
  def = Constant T.sqlDay

instance D.Default Constant Time.UTCTime (Column T.SqlTimestamptz) where
  def = Constant T.sqlUTCTime

instance D.Default Constant Time.LocalTime (Column T.SqlTimestamp) where
  def = Constant T.sqlLocalTime

instance D.Default Constant Time.ZonedTime (Column T.SqlTimestamptz) where
  def = Constant T.sqlZonedTime

instance D.Default Constant Time.TimeOfDay (Column T.SqlTime) where
  def = Constant T.sqlTimeOfDay

instance D.Default Constant (CI.CI ST.Text) (Column T.SqlCitext) where
  def = Constant T.sqlCiStrictText

instance D.Default Constant (CI.CI LT.Text) (Column T.SqlCitext) where
  def = Constant T.sqlCiLazyText

instance D.Default Constant SBS.ByteString (Column T.SqlJson) where
  def = Constant T.sqlStrictJSON

instance D.Default Constant LBS.ByteString (Column T.SqlJson) where
  def = Constant T.sqlLazyJSON

instance D.Default Constant Ae.Value (Column T.SqlJson) where
  def = Constant T.sqlValueJSON

instance D.Default Constant SBS.ByteString (Column T.SqlJsonb) where
  def = Constant T.sqlStrictJSONB

instance D.Default Constant LBS.ByteString (Column T.SqlJsonb) where
  def = Constant T.sqlLazyJSONB

instance D.Default Constant Ae.Value (Column T.SqlJsonb) where
  def = Constant T.sqlValueJSONB

instance D.Default Constant haskell (Column sql) => D.Default Constant (Maybe haskell) (Maybe (Column sql)) where
  def = Constant (constant <$>)

instance (D.Default Constant a (Column b), T.IsSqlType b)
         => D.Default Constant [a] (Column (T.SqlArray b)) where
  def = Constant (T.sqlArray (constantExplicit D.def))

instance D.Default Constant (R.PGRange Int.Int) (Column (T.SqlRange T.SqlInt4)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlInt4 a b

instance D.Default Constant (R.PGRange Int.Int64) (Column (T.SqlRange T.SqlInt8)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlInt8 a b

instance D.Default Constant (R.PGRange Sci.Scientific) (Column (T.SqlRange T.SqlNumeric)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlNumeric a b

instance D.Default Constant (R.PGRange Time.LocalTime) (Column (T.SqlRange T.SqlTimestamp)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlLocalTime a b

instance D.Default Constant (R.PGRange Time.UTCTime) (Column (T.SqlRange T.SqlTimestamptz)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlUTCTime a b

instance D.Default Constant (R.PGRange Time.Day) (Column (T.SqlRange T.SqlDate)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlDay a b

-- { Boilerplate instances

instance Functor (Constant a) where
  fmap f (Constant g) = Constant (fmap f g)

instance Applicative (Constant a) where
  pure = Constant . pure
  Constant f <*> Constant x = Constant (f <*> x)

instance P.Profunctor Constant where
  dimap f g (Constant h) = Constant (P.dimap f g h)

instance PP.ProductProfunctor Constant where
  empty = Constant empty
  Constant f ***! Constant g = Constant (f ***! g)

instance PP.SumProfunctor Constant where
  Constant f +++! Constant g = Constant (f +++! g)

-- }
