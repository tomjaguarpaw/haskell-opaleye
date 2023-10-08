{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Opaleye.Internal.Constant where

import           Opaleye.Field                   (Field)
import qualified Opaleye.Field                   as F
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

import qualified Database.PostgreSQL.Simple.Range as R
import           Database.PostgreSQL.Simple.Newtypes ( Aeson, getAeson )

toFields :: D.Default ToFields haskells fields
         => haskells -> fields
toFields = constantExplicit D.def

-- | A way of turning Haskell values of type @haskells@ into SQL
-- fields.  Use it with 'Opaleye.ToFields.toFields'.
newtype ToFields haskells fields =
  ToFields { constantExplicit :: haskells -> fields }

instance D.Default ToFields haskell (F.Field sql)
         => D.Default ToFields (Maybe haskell) (F.FieldNullable sql) where
  def = ToFields (F.maybeToNullable . fmap f)
    where ToFields f = D.def

toToFields :: (haskells -> fields) -> ToFields haskells fields
toToFields = ToFields

instance D.Default ToFields (Field a) (Field a) where
  def = toToFields id

instance D.Default ToFields String (Field T.SqlText) where
  def = toToFields T.sqlString

instance D.Default ToFields LBS.ByteString (Field T.SqlBytea) where
  def = toToFields T.sqlLazyByteString

instance D.Default ToFields SBS.ByteString (Field T.SqlBytea) where
  def = toToFields T.sqlStrictByteString

instance D.Default ToFields ST.Text (Field T.SqlText) where
  def = toToFields T.sqlStrictText

instance D.Default ToFields LT.Text (Field T.SqlText) where
  def = toToFields T.sqlLazyText

instance D.Default ToFields String (Field T.SqlVarcharN) where
  def = toToFields T.sqlStringVarcharN

instance D.Default ToFields ST.Text (Field T.SqlVarcharN) where
  def = toToFields T.sqlStrictTextVarcharN

instance D.Default ToFields LT.Text (Field T.SqlVarcharN) where
  def = toToFields T.sqlLazyTextVarcharN

instance D.Default ToFields Sci.Scientific (Field T.SqlNumeric) where
  def = toToFields T.sqlNumeric

instance D.Default ToFields Int (Field T.SqlInt4) where
  def = toToFields T.sqlInt4

instance D.Default ToFields Int.Int32 (Field T.SqlInt4) where
  def = toToFields $ T.sqlInt4 . fromIntegral

instance D.Default ToFields Int.Int64 (Field T.SqlInt8) where
  def = toToFields T.sqlInt8

instance D.Default ToFields Double (Field T.SqlFloat8) where
  def = toToFields T.sqlDouble

instance D.Default ToFields Bool (Field T.SqlBool) where
  def = toToFields T.sqlBool

instance D.Default ToFields UUID.UUID (Field T.SqlUuid) where
  def = toToFields T.sqlUUID

instance D.Default ToFields Time.Day (Field T.SqlDate) where
  def = toToFields T.sqlDay

instance D.Default ToFields Time.UTCTime (Field T.SqlTimestamptz) where
  def = toToFields T.sqlUTCTime

instance D.Default ToFields Time.LocalTime (Field T.SqlTimestamp) where
  def = toToFields T.sqlLocalTime

instance D.Default ToFields Time.ZonedTime (Field T.SqlTimestamptz) where
  def = toToFields T.sqlZonedTime

instance D.Default ToFields Time.TimeOfDay (Field T.SqlTime) where
  def = toToFields T.sqlTimeOfDay

instance D.Default ToFields Time.CalendarDiffTime (Field T.SqlInterval) where
  def = toToFields T.sqlInterval

instance D.Default ToFields (CI.CI ST.Text) (Field T.SqlCitext) where
  def = toToFields T.sqlCiStrictText

instance D.Default ToFields (CI.CI LT.Text) (Field T.SqlCitext) where
  def = toToFields T.sqlCiLazyText

instance D.Default ToFields SBS.ByteString (Field T.SqlJson) where
  def = toToFields T.sqlStrictJSON

instance D.Default ToFields LBS.ByteString (Field T.SqlJson) where
  def = toToFields T.sqlLazyJSON

instance D.Default ToFields Ae.Value (Field T.SqlJson) where
  def = toToFields T.sqlValueJSON

instance D.Default ToFields SBS.ByteString (Field T.SqlJsonb) where
  def = toToFields T.sqlStrictJSONB

instance D.Default ToFields LBS.ByteString (Field T.SqlJsonb) where
  def = toToFields T.sqlLazyJSONB

instance (Ae.ToJSON a) => D.Default ToFields (Aeson a) (Field T.SqlJson) where
  def = toToFields $ T.sqlValueJSON . getAeson

instance D.Default ToFields Ae.Value (Field T.SqlJsonb) where
  def = toToFields T.sqlValueJSONB

instance D.Default ToFields haskell (F.Field_ n sql) => D.Default ToFields (Maybe haskell) (Maybe (F.Field_ n sql)) where
  def = toToFields (toFields <$>)

instance (Ae.ToJSON a) => D.Default ToFields (Aeson a) (F.Field T.SqlJsonb) where
  def = toToFields $ T.sqlValueJSONB . getAeson

instance (D.Default ToFields a (F.Field_ n b), T.IsSqlType b)
         => D.Default ToFields [a] (F.Field (T.SqlArray_ n b)) where
  def = toToFields (T.sqlArray (constantExplicit D.def))

instance D.Default ToFields (R.PGRange Int.Int) (F.Field (T.SqlRange T.SqlInt4)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlInt4 a b

instance D.Default ToFields (R.PGRange Int.Int64) (F.Field (T.SqlRange T.SqlInt8)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlInt8 a b

instance D.Default ToFields (R.PGRange Sci.Scientific) (F.Field (T.SqlRange T.SqlNumeric)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlNumeric a b

instance D.Default ToFields (R.PGRange Time.LocalTime) (F.Field (T.SqlRange T.SqlTimestamp)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlLocalTime a b

instance D.Default ToFields (R.PGRange Time.UTCTime) (F.Field (T.SqlRange T.SqlTimestamptz)) where
  def = toToFields $ \(R.PGRange a b) -> T.sqlRange T.sqlUTCTime a b

instance D.Default ToFields (R.PGRange Time.Day) (F.Field (T.SqlRange T.SqlDate)) where
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
