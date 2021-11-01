{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Constant where

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
import qualified Data.Time.Compat                as Time
import qualified Data.UUID                       as UUID

import qualified Data.Profunctor.Product         as PP
import           Data.Profunctor.Product         (empty, (***!), (+++!))
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor                 as P

import           Control.Applicative (Applicative, pure, (<*>))
import           Data.Functor                    ((<$>))

import qualified Database.PostgreSQL.Simple.Range as R

toFields :: D.Default ToFields haskells fields
         => haskells -> fields
toFields = constantExplicit D.def

newtype ToFields haskells fields =
  ToFields { constantExplicit :: haskells -> fields }

instance D.Default ToFields haskell (Column sql)
         => D.Default ToFields (Maybe haskell) (Column (C.Nullable sql)) where
  def = ToFields (C.maybeToNullable . fmap f)
    where ToFields f = D.def

toToFields :: (haskells -> fields) -> ToFields haskells fields
toToFields = ToFields

instance D.Default ToFields (Column a) (Column a) where
  def = toToFields id

instance D.Default ToFields String (Column T.SqlText) where
  def = toToFields T.sqlString

instance D.Default ToFields LBS.ByteString (Column T.SqlBytea) where
  def = toToFields T.sqlLazyByteString

instance D.Default ToFields SBS.ByteString (Column T.SqlBytea) where
  def = toToFields T.sqlStrictByteString

instance D.Default ToFields ST.Text (Column T.SqlText) where
  def = toToFields T.sqlStrictText

instance D.Default ToFields LT.Text (Column T.SqlText) where
  def = toToFields T.sqlLazyText

instance D.Default ToFields String (Column T.SqlVarcharN) where
  def = toToFields T.sqlStringVarcharN

instance D.Default ToFields ST.Text (Column T.SqlVarcharN) where
  def = toToFields T.sqlStrictTextVarcharN

instance D.Default ToFields LT.Text (Column T.SqlVarcharN) where
  def = toToFields T.sqlLazyTextVarcharN

instance D.Default ToFields Sci.Scientific (Column T.SqlNumeric) where
  def = toToFields T.sqlNumeric

instance D.Default ToFields Int (Column T.SqlInt4) where
  def = toToFields T.sqlInt4

instance D.Default ToFields Int.Int32 (Column T.SqlInt4) where
  def = toToFields $ T.sqlInt4 . fromIntegral

instance D.Default ToFields Int.Int64 (Column T.SqlInt8) where
  def = toToFields T.sqlInt8

instance D.Default ToFields Double (Column T.SqlFloat8) where
  def = toToFields T.sqlDouble

instance D.Default ToFields Bool (Column T.SqlBool) where
  def = toToFields T.sqlBool

instance D.Default ToFields UUID.UUID (Column T.SqlUuid) where
  def = toToFields T.sqlUUID

instance D.Default ToFields Time.Day (Column T.SqlDate) where
  def = toToFields T.sqlDay

instance D.Default ToFields Time.UTCTime (Column T.SqlTimestamptz) where
  def = toToFields T.sqlUTCTime

instance D.Default ToFields Time.LocalTime (Column T.SqlTimestamp) where
  def = toToFields T.sqlLocalTime

instance D.Default ToFields Time.ZonedTime (Column T.SqlTimestamptz) where
  def = toToFields T.sqlZonedTime

instance D.Default ToFields Time.TimeOfDay (Column T.SqlTime) where
  def = toToFields T.sqlTimeOfDay

instance D.Default ToFields Time.CalendarDiffTime (Column T.SqlInterval) where
  def = toToFields T.sqlInterval

instance D.Default ToFields (CI.CI ST.Text) (Column T.SqlCitext) where
  def = toToFields T.sqlCiStrictText

instance D.Default ToFields (CI.CI LT.Text) (Column T.SqlCitext) where
  def = toToFields T.sqlCiLazyText

instance D.Default ToFields SBS.ByteString (Column T.SqlJson) where
  def = toToFields T.sqlStrictJSON

instance D.Default ToFields LBS.ByteString (Column T.SqlJson) where
  def = toToFields T.sqlLazyJSON

instance D.Default ToFields Ae.Value (Column T.SqlJson) where
  def = toToFields T.sqlValueJSON

instance D.Default ToFields SBS.ByteString (Column T.SqlJsonb) where
  def = toToFields T.sqlStrictJSONB

instance D.Default ToFields LBS.ByteString (Column T.SqlJsonb) where
  def = toToFields T.sqlLazyJSONB

instance D.Default ToFields Ae.Value (Column T.SqlJsonb) where
  def = toToFields T.sqlValueJSONB

instance D.Default ToFields haskell (Column sql) => D.Default ToFields (Maybe haskell) (Maybe (Column sql)) where
  def = toToFields (toFields <$>)

instance (D.Default ToFields a (Column b), T.IsSqlType b)
         => D.Default ToFields [a] (Column (T.SqlArray b)) where
  def = toToFields (T.sqlArray (constantExplicit D.def))

instance D.Default ToFields (R.PGRange Int.Int) (Column (T.SqlRange T.SqlInt4)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlInt4 a b

instance D.Default ToFields (R.PGRange Int.Int64) (Column (T.SqlRange T.SqlInt8)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlInt8 a b

instance D.Default ToFields (R.PGRange Sci.Scientific) (Column (T.SqlRange T.SqlNumeric)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlNumeric a b

instance D.Default ToFields (R.PGRange Time.LocalTime) (Column (T.SqlRange T.SqlTimestamp)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlLocalTime a b

instance D.Default ToFields (R.PGRange Time.UTCTime) (Column (T.SqlRange T.SqlTimestamptz)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlUTCTime a b

instance D.Default ToFields (R.PGRange Time.Day) (Column (T.SqlRange T.SqlDate)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlDay a b

-- { Boilerplate instances

instance Functor (ToFields a) where
  fmap f (ToFields g) = ToFields (fmap f g)

instance Applicative (ToFields a) where
  pure = ToFields . pure
  ToFields f <*> ToFields x = ToFields (f <*> x)

instance P.Profunctor ToFields where
  dimap f g (ToFields h) = ToFields (P.dimap f g h)

instance PP.ProductProfunctor ToFields where
  empty = ToFields empty
  ToFields f ***! ToFields g = ToFields (f ***! g)

instance PP.SumProfunctor ToFields where
  ToFields f +++! ToFields g = ToFields (f +++! g)

-- }
