{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Opaleye.Internal.RunQuery where

import           Control.Applicative
  (Applicative, pure, (<$>), (*>), (<*>), liftA2)

import qualified Database.PostgreSQL.Simple as PGS
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
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time as Time
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
data FromField pgType haskellType =
  QueryRunnerColumn (U.Unpackspec (Column pgType) ()) (FieldParser haskellType)

instance Functor (FromField u) where
  fmap f ~(QueryRunnerColumn u fp) = QueryRunnerColumn u ((fmap . fmap . fmap) f fp)

type QueryRunnerColumn = FromField

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
  QueryRunner (U.Unpackspec columns ())
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
              -- PGInt4)' has no columns when it is Nothing and one
              -- column when it is Just.

type QueryRunner = FromFields

fieldQueryRunnerColumn :: PGS.FromField haskell => FromField pgType haskell
fieldQueryRunnerColumn = fromPGSFromField

fromPGSFromField :: PGS.FromField haskell => FromField pgType haskell
fromPGSFromField = fieldParserQueryRunnerColumn fromField

fieldParserQueryRunnerColumn :: FieldParser haskell -> FromField pgType haskell
fieldParserQueryRunnerColumn = fromPGSFieldParser

fromPGSFieldParser :: FieldParser haskell -> FromField pgType haskell
fromPGSFieldParser = QueryRunnerColumn (P.rmap (const ()) U.unpackspecField)

queryRunner :: FromField a b -> FromFields (Column a) b
queryRunner qrc = QueryRunner u (const (fieldWith fp)) (const 1)
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
         DefaultFromField (Nullable a) (Maybe b) where
  defaultFromField = queryRunnerColumnNullable defaultFromField

instance DefaultFromField a b =>
         D.Default FromFields (Column a) b where
  def = queryRunner defaultFromField

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
{-# DEPRECATED queryRunnerColumnDefault "Use defaultFromField instead.  It will be removed in 0.8" #-}
class DefaultFromField sqlType haskellType where
  queryRunnerColumnDefault :: FromField sqlType haskellType
  queryRunnerColumnDefault = defaultFromField
  defaultFromField         :: FromField sqlType haskellType
  defaultFromField = queryRunnerColumnDefault

  {-# MINIMAL queryRunnerColumnDefault | defaultFromField #-}

type QueryRunnerColumnDefault = DefaultFromField

instance DefaultFromField sqlType haskellType
    => D.Default FromField sqlType haskellType where
  def = defaultFromField

instance DefaultFromField T.PGNumeric Sci.Scientific where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGInt4 Int where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGInt4 Int32 where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGInt8 Int64 where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGText String where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGFloat8 Double where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGBool Bool where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGUuid UUID where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGBytea SBS.ByteString where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGBytea LBS.ByteString where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGText ST.Text where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGText LT.Text where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGDate Time.Day where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGTimestamptz Time.UTCTime where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGTimestamp Time.LocalTime where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGTimestamptz Time.ZonedTime where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGTime Time.TimeOfDay where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGCitext (CI.CI ST.Text) where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGCitext (CI.CI LT.Text) where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGJson String where
  defaultFromField = fieldParserQueryRunnerColumn jsonFieldParser

instance DefaultFromField T.PGJson Ae.Value where
  defaultFromField = fromPGSFromField

instance DefaultFromField T.PGJsonb String where
  defaultFromField = fieldParserQueryRunnerColumn jsonbFieldParser

instance DefaultFromField T.PGJsonb Ae.Value where
  defaultFromField = fromPGSFromField

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

arrayColumn :: Column (T.PGArray a) -> Column a
arrayColumn = C.unsafeCoerceColumn

instance (Typeable b, DefaultFromField a b) =>
         DefaultFromField (T.PGArray a) [b] where
  defaultFromField = QueryRunnerColumn (P.lmap arrayColumn c) ((fmap . fmap . fmap) fromPGArray (pgArrayFieldParser f))
    where QueryRunnerColumn c f = defaultFromField

fromFieldArray :: Typeable h => FromField f h -> FromField (T.PGArray f) [h]
fromFieldArray q =
  QueryRunnerColumn (P.lmap arrayColumn c)
                    ((fmap . fmap . fmap) fromPGArray (pgArrayFieldParser f))
  where QueryRunnerColumn c f = q

-- }

instance (Typeable b, PGS.FromField b, DefaultFromField a b) =>
         DefaultFromField (T.PGRange a) (PGSR.PGRange b) where
  defaultFromField = fromPGSFromField

-- Boilerplate instances

instance Functor (FromFields c) where
  fmap f (QueryRunner u r b) = QueryRunner u ((fmap . fmap) f r) b

-- TODO: Seems like this one should be simpler!
instance Applicative (FromFields c) where
  pure = flip (QueryRunner (P.lmap (const ()) PP.empty)) (const 0)
         . pure
         . pure
  QueryRunner uf rf bf <*> QueryRunner ux rx bx =
    QueryRunner (P.dimap (\x -> (x,x)) (const ()) (uf PP.***! ux)) ((<*>) <$> rf <*> rx) (liftA2 (+) bf bx)

instance P.Profunctor FromFields where
  dimap f g (QueryRunner u r b) =
    QueryRunner (P.lmap f u) (P.dimap f (fmap g) r) (P.lmap f b)

instance PP.ProductProfunctor FromFields where
  purePP = pure
  (****) = (<*>)

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
prepareRowParser (QueryRunner _ rowParser numColumns) cols =
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
prepareQuery qr@(QueryRunner u _ _) q = (sql, parser)
  where sql :: Maybe PGS.Query
        sql = fmap String.fromString (S.showSqlExplicit u q)
        -- FIXME: We're doing work twice here
        (b, _, _) = Q.runSimpleQueryArrStart q ()
        parser = prepareRowParser qr b
