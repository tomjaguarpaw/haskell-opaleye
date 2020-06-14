{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Constant {-# DEPRECATED "Use \"Opaleye.ToFields\" instead.  Will be removed in version 0.8." #-} where

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

toFields :: D.Default ToFields haskells fields
         => haskells -> fields
toFields = constantExplicit D.def

{-# DEPRECATED constant "Use 'toFields' instead.  Will be removed in version 0.8." #-}
constant :: D.Default ToFields haskells fields
         => haskells -> fields
constant = constantExplicit D.def

newtype ToFields haskells fields =
  ToFields { constantExplicit :: haskells -> fields }

{-# DEPRECATED Constant "Use 'ToFields' instead.  Will be removed in version 0.8." #-}
type Constant = ToFields

instance D.Default ToFields haskell (Column sql)
         => D.Default ToFields (Maybe haskell) (Column (C.Nullable sql)) where
  def = ToFields (C.maybeToNullable . fmap f)
    where ToFields f = D.def

toToField :: (haskells -> fields) -> Constant haskells fields
toToField = ToFields

instance D.Default ToFields (Column a) (Column a) where
  def = Constant id

instance D.Default ToFields String (Column T.SqlText) where
  def = toToField T.sqlString

instance D.Default ToFields LBS.ByteString (Column T.SqlBytea) where
  def = toToField T.sqlLazyByteString

instance D.Default ToFields SBS.ByteString (Column T.SqlBytea) where
  def = toToField T.sqlStrictByteString

instance D.Default ToFields ST.Text (Column T.SqlText) where
  def = toToField T.sqlStrictText

instance D.Default ToFields LT.Text (Column T.SqlText) where
  def = toToField T.sqlLazyText

instance D.Default ToFields Sci.Scientific (Column T.SqlNumeric) where
  def = toToField T.sqlNumeric

instance D.Default ToFields Int (Column T.SqlInt4) where
  def = toToField T.sqlInt4

instance D.Default ToFields Int.Int32 (Column T.SqlInt4) where
  def = toToField $ T.sqlInt4 . fromIntegral

instance D.Default ToFields Int.Int64 (Column T.SqlInt8) where
  def = toToField T.sqlInt8

instance D.Default ToFields Double (Column T.SqlFloat8) where
  def = toToField T.sqlDouble

instance D.Default ToFields Bool (Column T.SqlBool) where
  def = toToField T.sqlBool

instance D.Default ToFields UUID.UUID (Column T.SqlUuid) where
  def = toToField T.sqlUUID

instance D.Default ToFields Time.Day (Column T.SqlDate) where
  def = toToField T.sqlDay

instance D.Default ToFields Time.UTCTime (Column T.SqlTimestamptz) where
  def = toToField T.sqlUTCTime

instance D.Default ToFields Time.LocalTime (Column T.SqlTimestamp) where
  def = toToField T.sqlLocalTime

instance D.Default ToFields Time.ZonedTime (Column T.SqlTimestamptz) where
  def = toToField T.sqlZonedTime

instance D.Default ToFields Time.TimeOfDay (Column T.SqlTime) where
  def = toToField T.sqlTimeOfDay

instance D.Default ToFields (CI.CI ST.Text) (Column T.SqlCitext) where
  def = toToField T.sqlCiStrictText

instance D.Default ToFields (CI.CI LT.Text) (Column T.SqlCitext) where
  def = toToField T.sqlCiLazyText

instance D.Default ToFields SBS.ByteString (Column T.SqlJson) where
  def = toToField T.sqlStrictJSON

instance D.Default ToFields LBS.ByteString (Column T.SqlJson) where
  def = toToField T.sqlLazyJSON

instance D.Default ToFields Ae.Value (Column T.SqlJson) where
  def = toToField T.sqlValueJSON

instance D.Default ToFields SBS.ByteString (Column T.SqlJsonb) where
  def = toToField T.sqlStrictJSONB

instance D.Default ToFields LBS.ByteString (Column T.SqlJsonb) where
  def = toToField T.sqlLazyJSONB

instance D.Default ToFields Ae.Value (Column T.SqlJsonb) where
  def = toToField T.sqlValueJSONB

instance D.Default ToFields haskell (Column sql) => D.Default ToFields (Maybe haskell) (Maybe (Column sql)) where
  def = toToField (constant <$>)

instance (D.Default ToFields a (Column b), T.IsSqlType b)
         => D.Default ToFields [a] (Column (T.SqlArray b)) where
  def = toToField (T.sqlArray (constantExplicit D.def))

instance D.Default ToFields (R.PGRange Int.Int) (Column (T.SqlRange T.SqlInt4)) where
  def = toToField $ \(R.PGRange a b) -> T.sqlRange T.sqlInt4 a b

instance D.Default ToFields (R.PGRange Int.Int64) (Column (T.SqlRange T.SqlInt8)) where
  def = toToField $ \(R.PGRange a b) -> T.sqlRange T.sqlInt8 a b

instance D.Default ToFields (R.PGRange Sci.Scientific) (Column (T.SqlRange T.SqlNumeric)) where
  def = toToField $ \(R.PGRange a b) -> T.sqlRange T.sqlNumeric a b

instance D.Default ToFields (R.PGRange Time.LocalTime) (Column (T.SqlRange T.SqlTimestamp)) where
  def = toToField $ \(R.PGRange a b) -> T.sqlRange T.sqlLocalTime a b

instance D.Default ToFields (R.PGRange Time.UTCTime) (Column (T.SqlRange T.SqlTimestamptz)) where
  def = toToField $ \(R.PGRange a b) -> T.sqlRange T.sqlUTCTime a b

instance D.Default ToFields (R.PGRange Time.Day) (Column (T.SqlRange T.SqlDate)) where
  def = toToField $ \(R.PGRange a b) -> T.sqlRange T.sqlDay a b

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
