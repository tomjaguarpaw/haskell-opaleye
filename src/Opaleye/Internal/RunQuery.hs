{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Opaleye.Internal.RunQuery where

import           Control.Applicative
  (Applicative, pure, (<$>), (*>), (<*>), liftA2)

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Cursor  as PGSC (Cursor)
import           Database.PostgreSQL.Simple.Internal (RowParser)
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Database.PostgreSQL.Simple.FromField
  (FieldParser, fromField, pgArrayFieldParser, optionalField)
import           Database.PostgreSQL.Simple.FromRow (fromRow, fieldWith)
import           Database.PostgreSQL.Simple.Types (fromPGArray, Only(..))

import           Opaleye.Internal.Column (Field_, Field, FieldNullable,
                                          Nullability(Nullable, NonNullable))
import qualified Opaleye.Internal.PackMap as PackMap
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.PGTypesExternal as T
import qualified Opaleye.Internal.PGTypes as IPT (strictDecodeUtf8)
import qualified Opaleye.Select as S
import qualified Opaleye.Sql as S

import qualified Data.Profunctor as P
import           Data.Profunctor (dimap)
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import qualified Data.Aeson as Ae
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as ST
import qualified Data.Text.Encoding as STE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Compat as Time
import qualified Data.Scientific as Sci
import qualified Data.String as String
import           Data.UUID (UUID)
import           GHC.Int (Int32, Int64)

-- { Only needed for postgresql-simple FieldParsers

import           Database.PostgreSQL.Simple.FromField
  (ResultError(UnexpectedNull, Incompatible), typeInfo, returnError)
import qualified Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.Range as PGSR
import           Data.Typeable (Typeable)

-- }

-- | A 'FromField' @sqlType@ @haskellType@
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

newtype FromField sqlType haskellType = FromField (FieldParser haskellType)

instance Functor (FromField u) where
  fmap f (FromField fp) = FromField ((fmap . fmap . fmap) f fp)

-- | A 'FromFields'
--   specifies how to convert Postgres values (@fields@)
--   into Haskell values (@haskells@).  Most likely you will never need
--   to create on of these or handle one directly.  It will be provided
--   for you by the 'D.Default' 'FromFields' instance.
--
-- \"'FromFields' @fields@ @haskells@\" corresponds to
-- postgresql-simple's \"'RowParser' @haskells@\".  \"'Default'
-- 'FromFields' @columns@ @haskells@\" corresponds to
-- postgresql-simple's \"@FromRow@ @haskells@\".
data FromFields columns haskells =
   FromFields (U.Unpackspec columns ())
              (columns -> RowParser haskells)
              -- We never actually look at the columns except to see
              -- its "type" in the case of a sum profunctor
              (columns -> Int)
              -- How many columns have we requested?  If we
              -- asked for zero columns then the SQL generator will
              -- have to put a dummy 0 into the SELECT statement,
              -- since we can't select zero columns.  In that case we
              -- have to make sure we read a single Int.
              --
              -- NB this does have to be a function of 'columns'
              -- because we have a `SumProfunctor` instance.  For some
              -- values of 'columns' there may be zero columns and for
              -- other values one or more, for example, 'Maybe (Column
              -- SqlInt4)' has no columns when it is Nothing and one
              -- column when it is Just.

{-# DEPRECATED fieldQueryRunnerColumn "Will be removed in version 0.10.  Use fromPGSFromField instead." #-}
fieldQueryRunnerColumn :: PGS.FromField haskell => FromField pgType haskell
fieldQueryRunnerColumn = fromPGSFromField

fromPGSFromField :: PGS.FromField haskell => FromField pgType haskell
fromPGSFromField = fromPGSFieldParser fromField

{-# DEPRECATED fieldParserQueryRunnerColumn " Will be removed in version 0.10.  Use fromPGSFieldParser instead." #-}
fieldParserQueryRunnerColumn :: FieldParser haskell -> FromField pgType haskell
fieldParserQueryRunnerColumn = fromPGSFieldParser

fromPGSFieldParser :: FieldParser haskell -> FromField pgType haskell
fromPGSFieldParser = FromField

fromFields :: FromField a b -> FromFields (Field a) b
fromFields (FromField fp) = fieldParserFromFields fp

fieldParserFromFields :: FieldParser haskells -> FromFields (Field_ n a) haskells
fieldParserFromFields fp = FromFields (P.rmap (const ()) U.unpackspecField) (const (fieldWith fp)) (const 1)

{-# DEPRECATED queryRunner "Use fromFields instead.  Will be removed in version 0.10." #-}
queryRunner :: FromField a b -> FromFields (Field a) b
queryRunner = fromFields

fromFieldsNullable :: FromField a b -> FromFields (FieldNullable a) (Maybe b)
fromFieldsNullable (FromField fp) = fieldParserFromFields (optionalField fp)

unsafeFromFieldRaw :: FromField a (PGS.Field, Maybe SBS.ByteString)
unsafeFromFieldRaw = fromPGSFieldParser (\f mdata -> pure (f, mdata))

-- { Instances for automatic derivation

instance DefaultFromField a b =>
         D.Default FromFields (Field a) b where
  def = fromFields defaultFromField

instance DefaultFromField a b =>
         D.Default FromFields (FieldNullable a) (Maybe b)
  where def = fromFieldsNullable defaultFromField


-- }

-- { Instances that must be provided once for each type.  Instances
--   for Nullable are derived automatically from these.

-- | A 'DefaultFromField' @sqlType@ @haskellType@ represents
-- the default way to turn a @sqlType@ result from the database into a
-- Haskell value of type @haskellType@.
--
-- \"'DefaultFromField' @sqlType@ @haskellType@\" corresponds
-- to postgresql-simple's \"'FromField' @haskellType@\".
--
-- Creating an instance of 'DefaultFromField' for your own types is
-- necessary for retrieving those types from the database.
--
-- You should use one of the three methods below for writing a
-- 'DefaultFromField' instance.
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
class DefaultFromField sqlType haskellType where
  defaultFromField         :: FromField sqlType haskellType

instance DefaultFromField sqlType haskellType
    => D.Default FromField sqlType haskellType where
  def = defaultFromField

instance DefaultFromField T.SqlNumeric Sci.Scientific where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlInt4 Int where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlInt4 Int32 where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlInt8 Int64 where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlText String where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlFloat8 Double where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlBool Bool where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlUuid UUID where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlBytea SBS.ByteString where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlBytea LBS.ByteString where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlText ST.Text where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlText LT.Text where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlVarcharN String where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlVarcharN ST.Text where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlVarcharN LT.Text where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlDate Time.Day where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlTimestamptz Time.UTCTime where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlTimestamp Time.LocalTime where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlTimestamptz Time.ZonedTime where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlInterval Time.CalendarDiffTime where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlTime Time.TimeOfDay where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlCitext (CI.CI ST.Text) where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlCitext (CI.CI LT.Text) where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlJson String where
  defaultFromField = fromPGSFieldParser jsonFieldParser

instance DefaultFromField T.SqlJson ST.Text where
  defaultFromField = fromPGSFieldParser jsonFieldTextParser

instance DefaultFromField T.SqlJson LT.Text where
  defaultFromField = fromPGSFieldParser jsonFieldLazyTextParser

instance DefaultFromField T.SqlJson SBS.ByteString where
  defaultFromField = fromPGSFieldParser jsonFieldByteParser

instance DefaultFromField T.SqlJson LBS.ByteString where
  defaultFromField = fromPGSFieldParser jsonFieldLazyByteParser

instance DefaultFromField T.SqlJson Ae.Value where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.SqlJsonb String where
  defaultFromField = fromPGSFieldParser jsonbFieldParser

instance DefaultFromField T.SqlJsonb ST.Text where
  defaultFromField = fromPGSFieldParser jsonbFieldTextParser

instance DefaultFromField T.SqlJsonb LT.Text where
  defaultFromField = fromPGSFieldParser jsonbFieldLazyTextParser

instance DefaultFromField T.SqlJsonb SBS.ByteString where
  defaultFromField = fromPGSFieldParser jsonbFieldByteParser

instance DefaultFromField T.SqlJsonb LBS.ByteString where
  defaultFromField = fromPGSFieldParser jsonbFieldLazyByteParser

instance DefaultFromField T.SqlJsonb Ae.Value where
  defaultFromField = fromPGSFromField

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

instance (Typeable b, DefaultFromField a b) =>
         DefaultFromField (T.SqlArray_ NonNullable a) [b] where
  defaultFromField = fromFieldArray defaultFromField

fromFieldArray :: Typeable h => FromField f h -> FromField (T.SqlArray_ NonNullable f) [h]
fromFieldArray (FromField f) =
  fmap fromPGArray (FromField (pgArrayFieldParser f))

fromFieldArrayNullable :: Typeable h => FromField f h -> FromField (T.SqlArray_ 'Nullable f) [Maybe h]
fromFieldArrayNullable (FromField f) =
  fmap fromPGArray (FromField (pgArrayFieldParser (optionalField f)))

-- }

instance (Typeable b, DefaultFromField a b) =>
         DefaultFromField (T.SqlRange a) (PGSR.PGRange b) where
  defaultFromField = fromFieldRange defaultFromField

fromFieldRange :: Typeable b
               => FromField a b
               -> FromField (T.SqlRange a) (PGSR.PGRange b)
fromFieldRange (FromField pff) = FromField (PGSR.fromFieldRange pff)

-- Boilerplate instances

instance Functor (FromFields c) where
  fmap f (FromFields u r b) = FromFields u ((fmap . fmap) f r) b

instance Applicative (FromFields c) where
  pure = flip (FromFields (pure ())) (const 0)
         . pure
         . pure
  FromFields uf rf bf <*> FromFields ux rx bx =
    FromFields (uf *> ux) ((<*>) <$> rf <*> rx) (liftA2 (+) bf bx)

instance P.Profunctor FromFields where
  dimap f g (FromFields u r b) =
    FromFields (P.lmap f u) (P.dimap f (fmap g) r) (P.lmap f b)

instance PP.ProductProfunctor FromFields where
  purePP = pure
  (****) = (<*>)

instance PP.SumProfunctor FromFields where
  f +++! g = FromFields (P.rmap (const ()) (fu PP.+++! gu))
                         (PackMap.eitherFunction fr gr)
                         (either fb gb)
    where FromFields fu fr fb = f
          FromFields gu gr gb = g

-- }

-- { Allow @postgresql-simple@ conversions from JSON types to 'String'

jsonFieldParser, jsonbFieldParser :: FieldParser String
jsonFieldParser  = jsonFieldTypeParser (String.fromString "json")
jsonbFieldParser = jsonFieldTypeParser (String.fromString "jsonb")

jsonFieldTextParser, jsonbFieldTextParser :: FieldParser ST.Text
jsonFieldTextParser  = jsonFieldTypeTextParser (String.fromString "json")
jsonbFieldTextParser = jsonFieldTypeTextParser (String.fromString "jsonb")

jsonFieldLazyTextParser, jsonbFieldLazyTextParser :: FieldParser LT.Text
jsonFieldLazyTextParser  = jsonFieldTypeLazyTextParser (String.fromString "json")
jsonbFieldLazyTextParser = jsonFieldTypeLazyTextParser (String.fromString "jsonb")

jsonFieldByteParser, jsonbFieldByteParser :: FieldParser SBS.ByteString
jsonFieldByteParser  = jsonFieldTypeByteParser (String.fromString "json")
jsonbFieldByteParser = jsonFieldTypeByteParser (String.fromString "jsonb")

jsonFieldLazyByteParser, jsonbFieldLazyByteParser :: FieldParser LBS.ByteString
jsonFieldLazyByteParser  = jsonFieldTypeLazyByteParser (String.fromString "json")
jsonbFieldLazyByteParser = jsonFieldTypeLazyByteParser (String.fromString "jsonb")

-- typenames, not type Oids are used in order to avoid creating
-- a dependency on 'Database.PostgreSQL.LibPQ'
--
-- Eventually we want to move this to postgresql-simple
--
--     https://github.com/tomjaguarpaw/haskell-opaleye/issues/329
jsonFieldTypeByteParser :: SBS.ByteString -> FieldParser SBS.ByteString
jsonFieldTypeByteParser jsonTypeName field mData = do
    ti <- typeInfo field
    if TI.typname ti == jsonTypeName
       then convert
       else returnError Incompatible field "types incompatible"
  where
    convert = case mData of
        Just bs -> pure bs
        _       -> returnError UnexpectedNull field ""

withJsonByteStringParser :: (SBS.ByteString -> b)
                         -> SBS.ByteString -> FieldParser b
withJsonByteStringParser f = (fmap . fmap . fmap . fmap) f jsonFieldTypeByteParser

jsonFieldTypeParser :: SBS.ByteString -> FieldParser String
jsonFieldTypeParser = withJsonByteStringParser IPT.strictDecodeUtf8

jsonFieldTypeTextParser :: SBS.ByteString -> FieldParser ST.Text
jsonFieldTypeTextParser = withJsonByteStringParser STE.decodeUtf8

jsonFieldTypeLazyTextParser :: SBS.ByteString -> FieldParser LT.Text
jsonFieldTypeLazyTextParser = withJsonByteStringParser (LTE.decodeUtf8 . LBS.fromStrict)

jsonFieldTypeLazyByteParser :: SBS.ByteString -> FieldParser LBS.ByteString
jsonFieldTypeLazyByteParser = withJsonByteStringParser LBS.fromStrict

-- }

prepareRowParser :: FromFields columns haskells -> columns -> RowParser haskells
prepareRowParser (FromFields _ rowParser numColumns) cols =
  if numColumns cols > 0
  then rowParser cols
  else (fromRow :: RowParser (Only Int)) *> rowParser cols
     -- If we are selecting zero columns then the SQL
     -- generator will have to put a dummy 0 into the
     -- SELECT statement, since we can't select zero
     -- columns.  In that case we have to make sure we
     -- read a single Int.

-- | Cursor within a transaction.
data Cursor haskells = EmptyCursor | Cursor (RowParser haskells) PGSC.Cursor

prepareQuery :: FromFields fields haskells -> S.Select fields -> (Maybe PGS.Query, RowParser haskells)
prepareQuery qr@(FromFields u _ _) q = (sql, parser)
  where sql :: Maybe PGS.Query
        sql = fmap String.fromString (S.showSqlExplicit u q)
        -- FIXME: We're doing work twice here
        (b, _, _) = Q.runSimpleQueryArrStart q ()
        parser = prepareRowParser qr b
