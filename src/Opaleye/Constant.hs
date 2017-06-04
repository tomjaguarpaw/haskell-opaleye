{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module Opaleye.Constant where

import           Opaleye.Column                  (Column, NullableColumn, Column')
import qualified Opaleye.Column                  as C
import qualified Opaleye.PGTypes                 as T

import qualified Data.Aeson                      as Ae
import qualified Data.CaseInsensitive            as CI
import qualified Data.Int                        as Int
import qualified Data.Text                       as ST
import qualified Data.Text.Lazy                  as LT
import qualified Data.ByteString                 as SBS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Time                       as Time
import qualified Data.UUID                       as UUID

import qualified Data.Profunctor.Product         as PP
import           Data.Profunctor.Product         (empty, (***!), (+++!))
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor                 as P

import           Control.Applicative (Applicative, pure, (<*>))
import           Data.Functor                    ((<$>))

import qualified Database.PostgreSQL.Simple.Range as R

-- | 'constant' provides a convenient typeclass wrapper around the
-- 'Column' creation functions in "Opaleye.PGTypes".  Besides
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
--   customInsert conn table haskells = 'Opaleye.Manipulation.runInsert' conn table $ 'constant' haskells
-- @
--
-- In order to use this function with your custom types, you need to define an
-- instance of 'D.Default' 'Constant' for your custom types.
constant :: D.Default Constant haskells columns
         => haskells -> columns
constant = constantExplicit D.def

newtype Constant haskells columns =
  Constant { constantExplicit :: haskells -> columns }

instance D.Default Constant haskell (Column sql)
         => D.Default Constant (Maybe haskell) (NullableColumn sql) where
  def = Constant (C.maybeToNullable . fmap f)
    where Constant f = D.def :: Constant haskell (Column sql)

instance D.Default Constant String (Column T.PGText) where
  def = Constant T.pgString

instance D.Default Constant LBS.ByteString (Column T.PGBytea) where
  def = Constant T.pgLazyByteString

instance D.Default Constant SBS.ByteString (Column T.PGBytea) where
  def = Constant T.pgStrictByteString

instance D.Default Constant ST.Text (Column T.PGText) where
  def = Constant T.pgStrictText

instance D.Default Constant LT.Text (Column T.PGText) where
  def = Constant T.pgLazyText

instance D.Default Constant Int (Column T.PGInt4) where
  def = Constant T.pgInt4

instance D.Default Constant Int.Int32 (Column T.PGInt4) where
  def = Constant $ T.pgInt4 . fromIntegral

instance D.Default Constant Int.Int64 (Column T.PGInt8) where
  def = Constant T.pgInt8

instance D.Default Constant Double (Column T.PGFloat8) where
  def = Constant T.pgDouble

instance D.Default Constant Bool (Column T.PGBool) where
  def = Constant T.pgBool

instance D.Default Constant UUID.UUID (Column T.PGUuid) where
  def = Constant T.pgUUID

instance D.Default Constant Time.Day (Column T.PGDate) where
  def = Constant T.pgDay

instance D.Default Constant Time.UTCTime (Column T.PGTimestamptz) where
  def = Constant T.pgUTCTime

instance D.Default Constant Time.LocalTime (Column T.PGTimestamp) where
  def = Constant T.pgLocalTime

instance D.Default Constant Time.TimeOfDay (Column T.PGTime) where
  def = Constant T.pgTimeOfDay

instance D.Default Constant (CI.CI ST.Text) (Column T.PGCitext) where
  def = Constant T.pgCiStrictText

instance D.Default Constant (CI.CI LT.Text) (Column T.PGCitext) where
  def = Constant T.pgCiLazyText

instance D.Default Constant SBS.ByteString (Column T.PGJson) where
  def = Constant T.pgStrictJSON

instance D.Default Constant LBS.ByteString (Column T.PGJson) where
  def = Constant T.pgLazyJSON

instance D.Default Constant Ae.Value (Column T.PGJson) where
  def = Constant T.pgValueJSON

instance D.Default Constant SBS.ByteString (Column T.PGJsonb) where
  def = Constant T.pgStrictJSONB

instance D.Default Constant LBS.ByteString (Column T.PGJsonb) where
  def = Constant T.pgLazyJSONB

instance D.Default Constant Ae.Value (Column T.PGJsonb) where
  def = Constant T.pgValueJSONB

instance D.Default Constant haskell (Column' n sql)
         => D.Default Constant (Maybe haskell) (Maybe (Column' n sql)) where
  def = Constant (constant <$>)

instance (D.Default Constant a (Column' n b), T.IsSqlType b)
         => D.Default Constant [a] (Column (T.PGArray n b)) where
  def = Constant (T.pgArray (constantExplicit D.def))

instance D.Default Constant (R.PGRange Int.Int) (Column (T.PGRange T.PGInt4)) where
  def = Constant $ \(R.PGRange a b) -> T.pgRange T.pgInt4 a b

instance D.Default Constant (R.PGRange Int.Int64) (Column (T.PGRange T.PGInt8)) where
  def = Constant $ \(R.PGRange a b) -> T.pgRange T.pgInt8 a b

-- TODO
--instance D.Default Constant (R.PGRange _) (Column (T.PGRange PGNumeric)) where

instance D.Default Constant (R.PGRange Time.LocalTime) (Column (T.PGRange T.PGTimestamp)) where
  def = Constant $ \(R.PGRange a b) -> T.pgRange T.pgLocalTime a b

instance D.Default Constant (R.PGRange Time.UTCTime) (Column (T.PGRange T.PGTimestamptz)) where
  def = Constant $ \(R.PGRange a b) -> T.pgRange T.pgUTCTime a b

instance D.Default Constant (R.PGRange Time.Day) (Column (T.PGRange T.PGDate)) where
  def = Constant $ \(R.PGRange a b) -> T.pgRange T.pgDay a b

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
