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
-- 'Opaleye.Field.Field_' creation functions in "Opaleye.SqlTypes".  Besides
-- convenience it doesn't provide any additional functionality.
--
-- It can be used with functions like 'Opaleye.Manipulation.runInsert'
-- to insert custom Haskell types into the database.
-- The following is an example of a function for inserting custom types.
--
-- @
--   customInsert
--      :: ( 'D.Default' 'ToFields' haskells fields )
--      => Connection
--      -> 'Opaleye.Table' fields fields'
--      -> haskells
--      -> IO Int64
--   customInsert conn table haskells = 'Opaleye.Manipulation.runInsert' conn table $ 'toFields' haskells
-- @
--
-- In order to use this function with your custom types, you need to define an
-- instance of 'D.Default' 'ToFields' for your custom types.
toFields :: D.Default ToFields haskells fields
         => haskells -> fields
toFields = constantExplicit D.def

-- | Do not use.  Use 'toFields' instead.  Will be deprecated in version 0.7.
constant :: D.Default ToFields haskells fields
         => haskells -> fields
constant = constantExplicit D.def

-- | Do not use the name @Constant@.  Use 'ToFields' instead.  Will be
-- deprecated in version 0.7.
newtype Constant haskells fields =
  Constant { constantExplicit :: haskells -> fields }

type ToFields = Constant

instance D.Default ToFields haskell (Column sql)
         => D.Default ToFields (Maybe haskell) (Column (C.Nullable sql)) where
  def = Constant (C.maybeToNullable . fmap f)
    where Constant f = D.def

instance D.Default ToFields String (Column T.SqlText) where
  def = Constant T.sqlString

instance D.Default ToFields LBS.ByteString (Column T.SqlBytea) where
  def = Constant T.sqlLazyByteString

instance D.Default ToFields SBS.ByteString (Column T.SqlBytea) where
  def = Constant T.sqlStrictByteString

instance D.Default ToFields ST.Text (Column T.SqlText) where
  def = Constant T.sqlStrictText

instance D.Default ToFields LT.Text (Column T.SqlText) where
  def = Constant T.sqlLazyText

instance D.Default ToFields Sci.Scientific (Column T.SqlNumeric) where
  def = Constant T.sqlNumeric

instance D.Default ToFields Int (Column T.SqlInt4) where
  def = Constant T.sqlInt4

instance D.Default ToFields Int.Int32 (Column T.SqlInt4) where
  def = Constant $ T.sqlInt4 . fromIntegral

instance D.Default ToFields Int.Int64 (Column T.SqlInt8) where
  def = Constant T.sqlInt8

instance D.Default ToFields Double (Column T.SqlFloat8) where
  def = Constant T.sqlDouble

instance D.Default ToFields Bool (Column T.SqlBool) where
  def = Constant T.sqlBool

instance D.Default ToFields UUID.UUID (Column T.SqlUuid) where
  def = Constant T.sqlUUID

instance D.Default ToFields Time.Day (Column T.SqlDate) where
  def = Constant T.sqlDay

instance D.Default ToFields Time.UTCTime (Column T.SqlTimestamptz) where
  def = Constant T.sqlUTCTime

instance D.Default ToFields Time.LocalTime (Column T.SqlTimestamp) where
  def = Constant T.sqlLocalTime

instance D.Default ToFields Time.ZonedTime (Column T.SqlTimestamptz) where
  def = Constant T.sqlZonedTime

instance D.Default ToFields Time.TimeOfDay (Column T.SqlTime) where
  def = Constant T.sqlTimeOfDay

instance D.Default ToFields (CI.CI ST.Text) (Column T.SqlCitext) where
  def = Constant T.sqlCiStrictText

instance D.Default ToFields (CI.CI LT.Text) (Column T.SqlCitext) where
  def = Constant T.sqlCiLazyText

instance D.Default ToFields SBS.ByteString (Column T.SqlJson) where
  def = Constant T.sqlStrictJSON

instance D.Default ToFields LBS.ByteString (Column T.SqlJson) where
  def = Constant T.sqlLazyJSON

instance D.Default ToFields Ae.Value (Column T.SqlJson) where
  def = Constant T.sqlValueJSON

instance D.Default ToFields SBS.ByteString (Column T.SqlJsonb) where
  def = Constant T.sqlStrictJSONB

instance D.Default ToFields LBS.ByteString (Column T.SqlJsonb) where
  def = Constant T.sqlLazyJSONB

instance D.Default ToFields Ae.Value (Column T.SqlJsonb) where
  def = Constant T.sqlValueJSONB

instance D.Default ToFields haskell (Column sql) => D.Default ToFields (Maybe haskell) (Maybe (Column sql)) where
  def = Constant (constant <$>)

instance (D.Default ToFields a (Column b), T.IsSqlType b)
         => D.Default ToFields [a] (Column (T.SqlArray b)) where
  def = Constant (T.sqlArray (constantExplicit D.def))

instance D.Default ToFields (R.PGRange Int.Int) (Column (T.SqlRange T.SqlInt4)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlInt4 a b

instance D.Default ToFields (R.PGRange Int.Int64) (Column (T.SqlRange T.SqlInt8)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlInt8 a b

instance D.Default ToFields (R.PGRange Sci.Scientific) (Column (T.SqlRange T.SqlNumeric)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlNumeric a b

instance D.Default ToFields (R.PGRange Time.LocalTime) (Column (T.SqlRange T.SqlTimestamp)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlLocalTime a b

instance D.Default ToFields (R.PGRange Time.UTCTime) (Column (T.SqlRange T.SqlTimestamptz)) where
  def = Constant $ \(R.PGRange a b) -> T.sqlRange T.sqlUTCTime a b

instance D.Default ToFields (R.PGRange Time.Day) (Column (T.SqlRange T.SqlDate)) where
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
