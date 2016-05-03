{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Constant where

import           Opaleye.Column                  (Column)
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


newtype Constant haskells columns =
  Constant { constantExplicit :: haskells -> columns }

constant :: D.Default Constant haskells columns
         => haskells -> columns
constant = constantExplicit D.def

instance D.Default Constant haskell (Column sql)
         => D.Default Constant (Maybe haskell) (Column (C.Nullable sql)) where
  def = Constant (C.maybeToNullable . fmap f)
    where Constant f = D.def

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

instance (D.Default Constant a (Column b), T.IsSqlType b)
         => D.Default Constant [a] (Column (T.PGArray b)) where
  def = Constant (T.pgArray (constantExplicit D.def))

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
