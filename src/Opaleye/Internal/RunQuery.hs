{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.Internal.RunQuery where

import           Control.Applicative (Applicative, pure, (*>), (<*>), liftA2)

import qualified Database.PostgreSQL.Simple.Cursor  as PGSC (Cursor)
import           Database.PostgreSQL.Simple.Internal (RowParser)
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Database.PostgreSQL.Simple.FromField
  (FieldParser, fromField, pgArrayFieldParser)
import           Database.PostgreSQL.Simple.FromRow (fromRow, fieldWith)
import           Database.PostgreSQL.Simple.Types (fromPGArray, Only(..))

import           Opaleye.Column (Column)
import           Opaleye.Internal.Column (Nullable)
import qualified Opaleye.Internal.PackMap as PackMap
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Map                 as Map
import qualified Opaleye.PGTypes as T
import qualified Opaleye.Internal.PGTypes as IPT (strictDecodeUtf8)

import qualified Data.Profunctor as P
import           Data.Profunctor (dimap)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product (empty, (***!))
import qualified Data.Profunctor.Product.Default as D

import qualified Data.Aeson as Ae
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time as Time
import qualified Data.Scientific as Sci
import qualified Data.String as String
import           Data.UUID (UUID)
import           GHC.Int (Int32, Int64)

-- { Only needed for postgresql-simple FieldParsers

import           Control.Applicative ((<$>))
import           Database.PostgreSQL.Simple.FromField
  (ResultError(UnexpectedNull, Incompatible), typeInfo, returnError)
import qualified Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.Range as PGSR
import           Data.Typeable (Typeable)

-- }

-- | A 'FromField' @sqlType@ @haskellType@
-- (or the old name, 'QueryRunnerColumn' @sqlType@ @haskellType@)
-- encodes how to turn
-- a value of Postgres type @sqlType@ into a value of Haskell type
-- @haskellType@.  For example a value of type 'FromField'
-- 'T.SqlText' 'String' encodes how to turn a 'T.SqlText' result from the
-- database into a Haskell 'String'.
--
-- \"'FromField' @sqlType@ @haskellType@\" corresponds to
-- postgresql-simple's \"'FieldParser' @haskellType@\".

-- This is *not* a Product Profunctor because it is the only way I
-- know of to get the instance generation to work for non-Nullable and
-- Nullable types at once.

-- I can no longer remember what the above comment means, but it might
-- be that we can't add nullability to a RowParser, only to a
-- FieldParser, so we have to have some type that we know contains
-- just a FieldParser.
data QueryRunnerColumn pgType haskellType =
  QueryRunnerColumn (U.Unpackspec (Column pgType) ()) (FieldParser haskellType)

instance Functor (FromField u) where
  fmap f ~(QueryRunnerColumn u fp) = QueryRunnerColumn u ((fmap . fmap . fmap) f fp)

type FromField = QueryRunnerColumn

-- | A 'FromFields' (or the old name 'QueryRunner')
--   specifies how to convert Postgres values (@fields@)
--   into Haskell values (@haskells@).  Most likely you will never need
--   to create on of these or handle one directly.  It will be provided
--   for you by the 'D.Default' 'FromFields' instance.
--
-- \"'FromFields' @fields@ @haskells@\" corresponds to
-- postgresql-simple's \"'RowParser' @haskells@\".  \"'Default'
-- 'FromFields' @columns@ @haskells@\" corresponds to
-- postgresql-simple's \"@FromRow@ @haskells@\".
data QueryRunner columns haskells =
  QueryRunner (U.Unpackspec columns ())
              (columns -> RowParser haskells)
              -- We never actually look at the columns except to see
              -- its "type" in the case of a sum profunctor
              (columns -> Bool)
              -- Have we actually requested any columns?  If we
              -- asked for zero columns then the SQL generator will
              -- have to put a dummy 0 into the SELECT statement,
              -- since we can't select zero columns.  In that case we
              -- have to make sure we read a single Int.
              --
              -- NB this does have to be a function of 'columns'
              -- because we have a `SumProfunctor` instance.  For some
              -- values of 'columns' there may be zero columns and for
              -- other values one or more, for example, 'Maybe (Column
              -- PGInt4)' has no columns when it is Nothing and one
              -- column when it is Just.

type FromFields = QueryRunner

fieldQueryRunnerColumn :: PGS.FromField haskell => FromField pgType haskell
fieldQueryRunnerColumn = fromPGSFromField

fromPGSFromField :: PGS.FromField haskell => FromField pgType haskell
fromPGSFromField = fieldParserQueryRunnerColumn fromField

fieldParserQueryRunnerColumn :: FieldParser haskell -> FromField pgType haskell
fieldParserQueryRunnerColumn = fromPGSFieldParser

fromPGSFieldParser :: FieldParser haskell -> FromField pgType haskell
fromPGSFieldParser = QueryRunnerColumn (P.rmap (const ()) U.unpackspecColumn)

queryRunner :: FromField a b -> FromFields (Column a) b
queryRunner qrc = QueryRunner u (const (fieldWith fp)) (const True)
    where QueryRunnerColumn u fp = qrc

queryRunnerColumnNullable :: FromField a b
                          -> FromField (Nullable a) (Maybe b)
queryRunnerColumnNullable qr =
  QueryRunnerColumn (P.lmap C.unsafeCoerceColumn u) (fromField' fp)
  where QueryRunnerColumn u fp = qr
        fromField' :: FieldParser a -> FieldParser (Maybe a)
        fromField' _ _ Nothing = pure Nothing
        fromField' fp' f bs = fmap Just (fp' f bs)

-- { Instances for automatic derivation

instance DefaultFromField a b =>
         QueryRunnerColumnDefault (Nullable a) (Maybe b) where
  defaultFromField = queryRunnerColumnNullable defaultFromField

instance DefaultFromField a b =>
         D.Default FromFields (Column a) b where
  def = queryRunner defaultFromField

-- }

-- { Instances that must be provided once for each type.  Instances
--   for Nullable are derived automatically from these.

-- | (Note that a better name for this class would be
-- 'DefaultFromField' and it probably will have this name in version
-- 0.7.  When you see 'QueryRunnerColumnDefault' please read
-- 'DefaultFromField' instead.  That will probably make things easier
-- to understand.)
--
-- A 'QueryRunnerColumnDefault' @sqlType@ @haskellType@ represents
-- the default way to turn a @sqlType@ result from the database into a
-- Haskell value of type @haskellType@.
--
-- \"'QueryRunnerColumnDefault' @sqlType@ @haskellType@\" corresponds
-- to postgresql-simple's \"'FromField' @haskellType@\".
--
-- Creating an instance of 'QueryRunnerColumnDefault' for your own types is
-- necessary for retrieving those types from the database.
--
-- You should use one of the three methods below for writing a
-- 'QueryRunnerColumnDefault' instance.
--
-- 1. If you already have a postgresql-simple 'PGS.FromField' instance for
-- your @haskellType@, use
-- 'fromPGSFromField'.  (This is how most of the built-in instances are
-- defined.)
--
-- 2. If you don't have a postgresql-simple 'PGS.FromField' instance, but
-- you do have an Opaleye 'FromField' value for the type it wraps use
-- 'Opaleye.RunSelect.unsafeFromField' if possible.  See the documentation for
-- 'Opaleye.RunSelect.unsafeFromField' for an example.
--
-- 3. If you have a more complicated case, but not a 'PGS.FromField' instance,
-- write a 'FieldParser' for your type and use 'fromPGSFieldParser'.
-- You can also add a 'FromField' instance using this.
class QueryRunnerColumnDefault sqlType haskellType where
  -- | Do not use @queryRunnerColumnDefault@.  It will be deprecated
  -- in version 0.7.
  queryRunnerColumnDefault :: FromField sqlType haskellType
  queryRunnerColumnDefault = defaultFromField
  defaultFromField         :: FromField sqlType haskellType
  defaultFromField = queryRunnerColumnDefault

  {-# MINIMAL queryRunnerColumnDefault | defaultFromField #-}

type DefaultFromField = QueryRunnerColumnDefault

instance DefaultFromField sqlType haskellType
    => D.Default FromField sqlType haskellType where
  def = defaultFromField

instance QueryRunnerColumnDefault T.PGNumeric Sci.Scientific where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGInt4 Int where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGInt4 Int32 where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGInt8 Int64 where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGText String where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGFloat8 Double where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGBool Bool where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGUuid UUID where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGBytea SBS.ByteString where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGBytea LBS.ByteString where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGText ST.Text where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGText LT.Text where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGDate Time.Day where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGTimestamptz Time.UTCTime where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGTimestamp Time.LocalTime where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGTimestamptz Time.ZonedTime where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGTime Time.TimeOfDay where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGCitext (CI.CI ST.Text) where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGCitext (CI.CI LT.Text) where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGJson String where
  defaultFromField = fieldParserQueryRunnerColumn jsonFieldParser

instance QueryRunnerColumnDefault T.PGJson Ae.Value where
  defaultFromField = fromPGSFromField

instance QueryRunnerColumnDefault T.PGJsonb String where
  defaultFromField = fieldParserQueryRunnerColumn jsonbFieldParser

instance QueryRunnerColumnDefault T.PGJsonb Ae.Value where
  defaultFromField = fromPGSFromField

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

arrayColumn :: Column (T.PGArray a) -> Column a
arrayColumn = C.unsafeCoerceColumn

instance (Typeable b, DefaultFromField a b) =>
         QueryRunnerColumnDefault (T.PGArray a) [b] where
  defaultFromField = QueryRunnerColumn (P.lmap arrayColumn c) ((fmap . fmap . fmap) fromPGArray (pgArrayFieldParser f))
    where QueryRunnerColumn c f = defaultFromField

-- }

instance (Typeable b, PGS.FromField b, DefaultFromField a b) =>
         QueryRunnerColumnDefault (T.PGRange a) (PGSR.PGRange b) where
  defaultFromField = fromPGSFromField

-- Boilerplate instances

instance Functor (FromFields c) where
  fmap f (QueryRunner u r b) = QueryRunner u ((fmap . fmap) f r) b

-- TODO: Seems like this one should be simpler!
instance Applicative (FromFields c) where
  pure = flip (QueryRunner (P.lmap (const ()) PP.empty)) (const False)
         . pure
         . pure
  QueryRunner uf rf bf <*> QueryRunner ux rx bx =
    QueryRunner (P.dimap (\x -> (x,x)) (const ()) (uf PP.***! ux)) ((<*>) <$> rf <*> rx) (liftA2 (||) bf bx)

instance P.Profunctor FromFields where
  dimap f g (QueryRunner u r b) =
    QueryRunner (P.lmap f u) (P.dimap f (fmap g) r) (P.lmap f b)

instance PP.ProductProfunctor FromFields where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor FromFields where
  f +++! g = QueryRunner (P.rmap (const ()) (fu PP.+++! gu))
                         (PackMap.eitherFunction fr gr)
                         (either fb gb)
    where QueryRunner fu fr fb = f
          QueryRunner gu gr gb = g

-- }

-- { Allow @postgresql-simple@ conversions from JSON types to 'String'

jsonFieldParser, jsonbFieldParser :: FieldParser String
jsonFieldParser  = jsonFieldTypeParser (String.fromString "json")
jsonbFieldParser = jsonFieldTypeParser (String.fromString "jsonb")

-- typenames, not type Oids are used in order to avoid creating
-- a dependency on 'Database.PostgreSQL.LibPQ'
--
-- Eventually we want to move this to postgresql-simple
--
--     https://github.com/tomjaguarpaw/haskell-opaleye/issues/329
jsonFieldTypeParser :: SBS.ByteString -> FieldParser String
jsonFieldTypeParser jsonTypeName field mData = do
    ti <- typeInfo field
    if TI.typname ti == jsonTypeName
       then convert
       else returnError Incompatible field "types incompatible"
  where
    convert = case mData of
        Just bs -> pure $ IPT.strictDecodeUtf8 bs
        _       -> returnError UnexpectedNull field ""

-- }

prepareRowParser :: FromFields columns haskells -> columns -> RowParser haskells
prepareRowParser (QueryRunner _ rowParser nonZeroColumns) cols =
  if nonZeroColumns cols
  then rowParser cols
  else (fromRow :: RowParser (Only Int)) *> rowParser cols
     -- If we are selecting zero columns then the SQL
     -- generator will have to put a dummy 0 into the
     -- SELECT statement, since we can't select zero
     -- columns.  In that case we have to make sure we
     -- read a single Int.

-- | Cursor within a transaction.
data Cursor haskells = EmptyCursor | Cursor (RowParser haskells) PGSC.Cursor

--

data HaskellToSql
data SqlToHaskell

type instance Map.Map HaskellToSql Int = Column (T.PGInt4)
type instance Map.Map HaskellToSql Int32 = Column (T.PGInt4)
type instance Map.Map HaskellToSql String = Column (T.PGText)
type instance Map.Map HaskellToSql Int64 = Column (T.PGInt8)
type instance Map.Map HaskellToSql Double = Column (T.PGFloat8)
type instance Map.Map HaskellToSql Bool = Column (T.PGBool)
type instance Map.Map HaskellToSql UUID = Column (T.PGUuid)
type instance Map.Map HaskellToSql SBS.ByteString = Column (T.PGBytea)
type instance Map.Map HaskellToSql LBS.ByteString = Column (T.PGBytea)
type instance Map.Map HaskellToSql ST.Text = Column (T.PGText)
type instance Map.Map HaskellToSql LT.Text = Column (T.PGText)
type instance Map.Map HaskellToSql Time.Day = Column (T.PGDate)
type instance Map.Map HaskellToSql Time.UTCTime = Column (T.PGTimestamptz)
type instance Map.Map HaskellToSql Time.LocalTime = Column (T.PGTimestamp)
type instance Map.Map HaskellToSql Time.ZonedTime = Column (T.PGTimestamptz)
type instance Map.Map HaskellToSql Time.TimeOfDay = Column (T.PGTime)
type instance Map.Map HaskellToSql (CI.CI ST.Text) = Column (T.PGCitext)
type instance Map.Map HaskellToSql (CI.CI LT.Text) = Column (T.PGCitext)
type instance Map.Map HaskellToSql Ae.Value = Column (T.PGJson)

type instance Map.Map HaskellToSql (Maybe Int) = Column (Nullable (T.PGInt4))
type instance Map.Map HaskellToSql (Maybe Int32) = Column (Nullable (T.PGInt4))
type instance Map.Map HaskellToSql (Maybe String) = Column (Nullable (T.PGText))
type instance Map.Map HaskellToSql (Maybe Int64) = Column (Nullable (T.PGInt8))
type instance Map.Map HaskellToSql (Maybe Double) = Column (Nullable (T.PGFloat8))
type instance Map.Map HaskellToSql (Maybe Bool) = Column (Nullable (T.PGBool))
type instance Map.Map HaskellToSql (Maybe UUID) = Column (Nullable (T.PGUuid))
type instance Map.Map HaskellToSql (Maybe SBS.ByteString) = Column (Nullable (T.PGBytea))
type instance Map.Map HaskellToSql (Maybe LBS.ByteString) = Column (Nullable (T.PGBytea))
type instance Map.Map HaskellToSql (Maybe ST.Text) = Column (Nullable (T.PGText))
type instance Map.Map HaskellToSql (Maybe LT.Text) = Column (Nullable (T.PGText))
type instance Map.Map HaskellToSql (Maybe Time.Day) = Column (Nullable (T.PGDate))
type instance Map.Map HaskellToSql (Maybe Time.UTCTime) = Column (Nullable (T.PGTimestamptz))
type instance Map.Map HaskellToSql (Maybe Time.LocalTime) = Column (Nullable (T.PGTimestamp))
type instance Map.Map HaskellToSql (Maybe Time.ZonedTime) = Column (Nullable (T.PGTimestamptz))
type instance Map.Map HaskellToSql (Maybe Time.TimeOfDay) = Column (Nullable (T.PGTime))
type instance Map.Map HaskellToSql (Maybe (CI.CI ST.Text)) = Column (Nullable (T.PGCitext))
type instance Map.Map HaskellToSql (Maybe (CI.CI LT.Text)) = Column (Nullable (T.PGCitext))
type instance Map.Map HaskellToSql (Maybe Ae.Value) = Column (Nullable (T.PGJson))

type instance Map.Map SqlToHaskell (Column T.PGInt4) = Int
type instance Map.Map SqlToHaskell (Column T.PGInt8) = Int64
type instance Map.Map SqlToHaskell (Column T.PGText) = ST.Text
type instance Map.Map SqlToHaskell (Column T.PGFloat8) = Double
type instance Map.Map SqlToHaskell (Column T.PGBool) = Bool
type instance Map.Map SqlToHaskell (Column T.PGUuid) = UUID
type instance Map.Map SqlToHaskell (Column T.PGBytea) = SBS.ByteString
type instance Map.Map SqlToHaskell (Column T.PGText) = ST.Text
type instance Map.Map SqlToHaskell (Column T.PGDate) = Time.Day
type instance Map.Map SqlToHaskell (Column T.PGTimestamp) = Time.LocalTime
type instance Map.Map SqlToHaskell (Column T.PGTimestamptz) = Time.ZonedTime
type instance Map.Map SqlToHaskell (Column T.PGTime) = Time.TimeOfDay
type instance Map.Map SqlToHaskell (Column T.PGCitext) = (CI.CI ST.Text)
type instance Map.Map SqlToHaskell (Column T.PGJson) = Ae.Value
type instance Map.Map SqlToHaskell (Column T.PGJsonb) = Ae.Value

type instance Map.Map SqlToHaskell (Column (Nullable T.PGInt4)) = Maybe Int
type instance Map.Map SqlToHaskell (Column (Nullable T.PGInt8)) = Maybe Int64
type instance Map.Map SqlToHaskell (Column (Nullable T.PGText)) = Maybe ST.Text
type instance Map.Map SqlToHaskell (Column (Nullable T.PGFloat8)) = Maybe Double
type instance Map.Map SqlToHaskell (Column (Nullable T.PGBool)) = Maybe Bool
type instance Map.Map SqlToHaskell (Column (Nullable T.PGUuid)) = Maybe UUID
type instance Map.Map SqlToHaskell (Column (Nullable T.PGBytea)) = Maybe SBS.ByteString
type instance Map.Map SqlToHaskell (Column (Nullable T.PGText)) = Maybe ST.Text
type instance Map.Map SqlToHaskell (Column (Nullable T.PGDate)) = Maybe Time.Day
type instance Map.Map SqlToHaskell (Column (Nullable T.PGTimestamp)) = Maybe Time.LocalTime
type instance Map.Map SqlToHaskell (Column (Nullable T.PGTimestamptz)) = Maybe Time.ZonedTime
type instance Map.Map SqlToHaskell (Column (Nullable T.PGTime)) = Maybe Time.TimeOfDay
type instance Map.Map SqlToHaskell (Column (Nullable T.PGCitext)) = Maybe (CI.CI ST.Text)
type instance Map.Map SqlToHaskell (Column (Nullable T.PGJson)) = Maybe Ae.Value
type instance Map.Map SqlToHaskell (Column (Nullable T.PGJsonb)) = Maybe Ae.Value
