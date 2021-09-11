{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Opaleye.Internal.Inferrable where

import qualified Opaleye.Column as C
import           Opaleye.Internal.RunQuery (FromField, FromFields)
import qualified Opaleye.Internal.RunQuery as RQ
import qualified Opaleye.SqlTypes as T
import           Opaleye.Internal.Constant (ToFields)

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Scientific                 as Sci
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Time.Compat as Time
import           Data.Typeable (Typeable)
import           Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple.Range as R
import           GHC.Int (Int32, Int64)

-- | Despite its name, 'Inferrable' doesn't provide any inferability
-- improvements itself, it's just a conveniently-named newtype wrapper
-- to hang instances with better inferrability off of.
newtype Inferrable p a b = Inferrable { runInferrable :: p a b }

-- FromFields

-- OVERLAPPABLE is pretty grim, but it will go away when we switch to Field
instance {-# OVERLAPPABLE #-}
  D.Default (Inferrable FromField) a b
  => D.Default (Inferrable FromFields) (C.Column a) b where
  def = Inferrable (RQ.queryRunner (runInferrable D.def))

instance
     (D.Default (Inferrable FromField) a b, Maybe b ~ maybe_b)
  => D.Default (Inferrable FromFields) (C.Column (C.Nullable a)) maybe_b where
  def = Inferrable (RQ.queryRunner (RQ.queryRunnerColumnNullable (runInferrable D.def)))

-- FromField

instance int ~ Int => D.Default (Inferrable FromField) T.SqlInt4 int where
  def = Inferrable D.def

instance int64 ~ Int64 => D.Default (Inferrable FromField) T.SqlInt8 int64 where
  def = Inferrable D.def

instance text ~ ST.Text => D.Default (Inferrable FromField) T.SqlText text where
  def = Inferrable D.def

instance varchar ~ ST.Text => D.Default (Inferrable FromField) T.SqlVarcharN varchar where
  def = Inferrable D.def

instance (Typeable h, D.Default (Inferrable FromField) f h, hs ~ [h])
  => D.Default (Inferrable FromField) (T.SqlArray f) hs where
  def = Inferrable (RQ.fromFieldArray (runInferrable D.def))

instance double ~ Double => D.Default (Inferrable FromField) T.SqlFloat8 double where
  def = Inferrable D.def

instance scientific ~ Sci.Scientific
  => D.Default (Inferrable FromField) T.SqlNumeric scientific where
  def = Inferrable D.def

instance bool ~ Bool => D.Default (Inferrable FromField) T.SqlBool bool where
  def = Inferrable D.def

instance uuid ~ UUID => D.Default (Inferrable FromField) T.SqlUuid uuid where
  def = Inferrable D.def

instance bytestring ~ SBS.ByteString
  => D.Default (Inferrable FromField) T.SqlBytea bytestring where
  def = Inferrable D.def

instance day ~ Time.Day
  => D.Default (Inferrable FromField) T.SqlDate day where
  def = Inferrable D.def

-- I'm not certain what we should map timestamptz to.  The
-- postgresql-simple types it maps to are ZonedTime and UTCTime, but
-- maybe it's more accurate to map it to a *pair* of LocalTime and a
-- time zone.

--instance utctime ~ Time.UTCTime
--  => D.Default (Inferrable FromField) T.SqlTimestamptz utctime where
--  def = Inferrable D.def

instance localtime ~ Time.LocalTime
  => D.Default (Inferrable FromField) T.SqlTimestamp localtime where
  def = Inferrable D.def

instance timeofday ~ Time.TimeOfDay
  => D.Default (Inferrable FromField) T.SqlTime timeofday where
  def = Inferrable D.def

instance calendardifftime ~ Time.CalendarDiffTime
  => D.Default (Inferrable FromField) T.SqlInterval calendardifftime where
  def = Inferrable D.def

instance cttext ~ CI.CI ST.Text
  => D.Default (Inferrable FromField) T.SqlCitext cttext where
  def = Inferrable D.def

-- It's not clear what to map JSON types to

{-
instance QueryRunnerColumnDefault T.PGJson String where
  defaultFromField = fieldParserQueryRunnerColumn jsonFieldParser

instance QueryRunnerColumnDefault T.PGJson Ae.Value where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGJsonb String where
  defaultFromField = fieldParserQueryRunnerColumn jsonbFieldParser

instance QueryRunnerColumnDefault T.PGJsonb Ae.Value where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGTimestamptz Time.UTCTime where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGTimestamptz Time.ZonedTime where
  defaultFromField = fromPGSFromField
-}

-- ToFields

{- The instance for arrays would clash with String.  String is going to
   be use far more, so to get arrays you'll have to explicitly use
   `sqlArray`.

instance (D.Default (Inferrable ToFields) a (C.Column b),
          T.IsSqlType b,
          C.Column (T.SqlArray b) ~ cSqlArrayb)
         => D.Default (Inferrable ToFields) [a] cSqlArrayb where
  def = Inferrable (toToFields (T.sqlArray (toFieldsExplicit
                                               (runInferrable D.def))))
-}

instance C.Column a ~ columnA
  => D.Default (Inferrable ToFields) (C.Column a) columnA where
  def = Inferrable D.def

instance C.Column T.SqlText ~ cSqlText
  => D.Default (Inferrable ToFields) String cSqlText where
  def = Inferrable D.def

instance C.Column T.SqlBytea ~ cSqlBytea
  => D.Default (Inferrable ToFields) LBS.ByteString cSqlBytea where
  def = Inferrable D.def

instance C.Column T.SqlBytea ~ cSqlBytea
  => D.Default (Inferrable ToFields) SBS.ByteString cSqlBytea where
  def = Inferrable D.def

instance C.Column T.SqlText ~ cSqlText
  => D.Default (Inferrable ToFields) ST.Text cSqlText where
  def = Inferrable D.def

instance C.Column T.SqlText ~ cSqlText
  => D.Default (Inferrable ToFields) LT.Text cSqlText where
  def = Inferrable D.def

instance C.Column T.SqlNumeric ~ cSqlNumeric
  => D.Default (Inferrable ToFields) Sci.Scientific cSqlNumeric where
  def = Inferrable D.def

instance C.Column T.SqlInt4 ~ cSqlInt4
  => D.Default (Inferrable ToFields) Int cSqlInt4 where
  def = Inferrable D.def

instance C.Column T.SqlInt4 ~ cSqlInt4
  => D.Default (Inferrable ToFields) Int32 cSqlInt4 where
  def = Inferrable D.def

instance C.Column T.SqlInt8 ~ cSqlInt8
  => D.Default (Inferrable ToFields) Int64 cSqlInt8 where
  def = Inferrable D.def

instance C.Column T.SqlFloat8 ~ cSqlFloat8
  => D.Default (Inferrable ToFields) Double cSqlFloat8 where
  def = Inferrable D.def

instance C.Column T.SqlBool ~ cSqlBool
  => D.Default (Inferrable ToFields) Bool cSqlBool where
  def = Inferrable D.def

instance C.Column T.SqlUuid ~ cSqlUuid
  => D.Default (Inferrable ToFields) UUID cSqlUuid where
  def = Inferrable D.def

instance C.Column T.SqlDate ~ cSqlDate
  => D.Default (Inferrable ToFields) Time.Day cSqlDate where
  def = Inferrable D.def

instance C.Column T.SqlTimestamptz ~ cSqlTimestamptz
  => D.Default (Inferrable ToFields) Time.UTCTime cSqlTimestamptz where
  def = Inferrable D.def

instance C.Column T.SqlTimestamptz ~ cSqlTimestamptz
  => D.Default (Inferrable ToFields) Time.ZonedTime cSqlTimestamptz where
  def = Inferrable D.def

instance C.Column T.SqlTime ~ cSqlTime
  => D.Default (Inferrable ToFields) Time.TimeOfDay cSqlTime where
  def = Inferrable D.def

instance C.Column T.SqlInterval ~ cSqlInterval
  => D.Default (Inferrable ToFields) Time.CalendarDiffTime cSqlInterval where
  def = Inferrable D.def

instance C.Column T.SqlCitext ~ cSqlCitext
  => D.Default (Inferrable ToFields) (CI.CI ST.Text) cSqlCitext where
  def = Inferrable D.def

instance C.Column T.SqlCitext ~ cSqlCitext
  => D.Default (Inferrable ToFields) (CI.CI LT.Text) cSqlCitext where
  def = Inferrable D.def

instance C.Column (T.SqlRange T.SqlInt4) ~ cRangeInt4
  => D.Default (Inferrable ToFields) (R.PGRange Int) cRangeInt4 where
  def = Inferrable D.def

instance C.Column (T.SqlRange T.SqlInt8) ~ cRangeInt8
  => D.Default (Inferrable ToFields) (R.PGRange Int64) cRangeInt8 where
  def = Inferrable D.def

instance C.Column (T.SqlRange T.SqlNumeric) ~ cRangeScientific
  => D.Default (Inferrable ToFields) (R.PGRange Sci.Scientific) cRangeScientific where
  def = Inferrable D.def

instance C.Column (T.SqlRange T.SqlTimestamp) ~ cRangeTimestamp
  => D.Default (Inferrable ToFields) (R.PGRange Time.LocalTime) cRangeTimestamp where
  def = Inferrable D.def

instance C.Column (T.SqlRange T.SqlTimestamptz) ~ cRangeTimestamptz
  => D.Default (Inferrable ToFields) (R.PGRange Time.UTCTime) cRangeTimestamptz where
  def = Inferrable D.def

instance C.Column (T.SqlRange T.SqlDate) ~ cRangeDate
  => D.Default (Inferrable ToFields) (R.PGRange Time.Day) cRangeDate where
  def = Inferrable D.def

{-  It's not clear if Aeson Value should map to JSON or JSONB.

instance D.Default ToFields Ae.Value (Column T.SqlJson) where
  def = Constant T.sqlValueJSON

instance D.Default ToFields Ae.Value (Column T.SqlJsonb) where
  def = Constant T.sqlValueJSONB

-}

-- Boilerplate instances

instance Functor (p a) => Functor (Inferrable p a) where
  fmap f = Inferrable . fmap f . runInferrable

instance Applicative (p a) => Applicative (Inferrable p a) where
  pure = Inferrable . pure
  f <*> x = Inferrable (runInferrable f <*> runInferrable x)

instance P.Profunctor p => P.Profunctor (Inferrable p) where
  dimap f g = Inferrable . P.dimap f g . runInferrable

instance PP.ProductProfunctor p => PP.ProductProfunctor (Inferrable p) where
  purePP = Inferrable . PP.purePP
  f **** g = Inferrable (runInferrable f PP.**** runInferrable g)
