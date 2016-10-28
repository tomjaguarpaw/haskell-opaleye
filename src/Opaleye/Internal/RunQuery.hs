{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.RunQuery where

import           Control.Applicative (Applicative, pure, (*>), (<*>), liftA2)

import           Database.PostgreSQL.Simple.Internal (RowParser)
import           Database.PostgreSQL.Simple.FromField (FieldParser, FromField,
                                                       fromField)
import           Database.PostgreSQL.Simple.FromRow (fromRow, fieldWith)
import           Database.PostgreSQL.Simple.Types (fromPGArray, Only(..))

import           Opaleye.Column (Column)
import           Opaleye.Internal.Column (Nullable)
import qualified Opaleye.Internal.PackMap as PackMap
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Unpackspec as U
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
import qualified Data.String as String
import           Data.UUID (UUID)
import           GHC.Int (Int32, Int64)

-- { Only needed for annoying postgresql-simple patch below

import           Control.Applicative ((<$>))
import           Database.PostgreSQL.Simple.FromField
  (Field, typoid, typeOid, typelem, TypeInfo,
   ResultError(UnexpectedNull, ConversionFailed, Incompatible),
   typdelim, typeInfo, returnError, Conversion)
import           Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import           Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import qualified Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.Arrays as Arrays
import           Database.PostgreSQL.Simple.Arrays (array, fmt)
import qualified Database.PostgreSQL.Simple.Range as PGSR
import           Data.String (fromString)
import           Data.Typeable (Typeable)

-- }

-- | A 'QueryRunnerColumn' @pgType@ @haskellType@ encodes how to turn
-- a value of Postgres type @pgType@ into a value of Haskell type
-- @haskellType@.  For example a value of type 'QueryRunnerColumn'
-- 'T.PGText' 'String' encodes how to turn a 'T.PGText' result from the
-- database into a Haskell 'String'.

-- This is *not* a Product Profunctor because it is the only way I
-- know of to get the instance generation to work for non-Nullable and
-- Nullable types at once.

-- I can no longer remember what the above comment means, but it might
-- be that we can't add nullability to a RowParser, only to a
-- FieldParser, so we have to have some type that we know contains
-- just a FieldParser.
data QueryRunnerColumn pgType haskellType =
  QueryRunnerColumn (U.Unpackspec (Column pgType) ()) (FieldParser haskellType)

instance Functor (QueryRunnerColumn u) where
  fmap f ~(QueryRunnerColumn u fp) = QueryRunnerColumn u ((fmap . fmap . fmap) f fp)

data QueryRunner columns haskells =
  QueryRunner (U.Unpackspec columns ())
              (columns -> RowParser haskells)
              -- We never actually look at the columns except to see
              -- its "type" in the case of a sum profunctor
              (columns -> Bool)
              -- ^ Have we actually requested any columns?  If we
              -- asked for zero columns then the SQL generator will
              -- have to put a dummy 0 into the SELECT statement,
              -- since we can't select zero columns.  In that case we
              -- have to make sure we read a single Int.

fieldQueryRunnerColumn :: FromField haskell => QueryRunnerColumn pgType haskell
fieldQueryRunnerColumn = fieldParserQueryRunnerColumn fromField

fieldParserQueryRunnerColumn :: FieldParser haskell -> QueryRunnerColumn pgType haskell
fieldParserQueryRunnerColumn = QueryRunnerColumn (P.rmap (const ()) U.unpackspecColumn)

queryRunner :: QueryRunnerColumn a b -> QueryRunner (Column a) b
queryRunner qrc = QueryRunner u (const (fieldWith fp)) (const True)
    where QueryRunnerColumn u fp = qrc

queryRunnerColumnNullable :: QueryRunnerColumn a b
                          -> QueryRunnerColumn (Nullable a) (Maybe b)
queryRunnerColumnNullable qr =
  QueryRunnerColumn (P.lmap C.unsafeCoerceColumn u) (fromField' fp)
  where QueryRunnerColumn u fp = qr
        fromField' :: FieldParser a -> FieldParser (Maybe a)
        fromField' _ _ Nothing = pure Nothing
        fromField' fp' f bs = fmap Just (fp' f bs)

-- { Instances for automatic derivation

instance QueryRunnerColumnDefault a b =>
         QueryRunnerColumnDefault (Nullable a) (Maybe b) where
  queryRunnerColumnDefault = queryRunnerColumnNullable queryRunnerColumnDefault

instance QueryRunnerColumnDefault a b =>
         D.Default QueryRunner (Column a) b where
  def = queryRunner queryRunnerColumnDefault

-- }

-- { Instances that must be provided once for each type.  Instances
--   for Nullable are derived automatically from these.

-- | A 'QueryRunnerColumnDefault' @pgType@ @haskellType@ represents
-- the default way to turn a @pgType@ result from the database into a
-- Haskell value of type @haskellType@.
--
-- Creating an instance of 'QueryRunnerColumnDefault' for your own types is
-- necessary for retrieving those types from the database.
--
-- You should use one of the three methods below for writing a
-- 'QueryRunnerColumnDefault' instance.
--
-- 1. If you already have a 'FromField' instance for your @haskellType@, use
-- 'fieldQueryRunnerColumn'.  (This is how most of the built-in instances are
-- defined.)
--
-- 2. If you don't have a 'FromField' instance, use
-- 'Opaleye.RunQuery.queryRunnerColumn' if possible.  See the documentation for
-- 'Opaleye.RunQuery.queryRunnerColumn' for an example.
--
-- 3. If you have a more complicated case, but not a 'FromField' instance,
-- write a 'FieldParser' for your type and use 'fieldParserQueryRunnerColumn'.
-- You can also add a 'FromField' instance using this.
class QueryRunnerColumnDefault pgType haskellType where
  queryRunnerColumnDefault :: QueryRunnerColumn pgType haskellType

instance QueryRunnerColumnDefault T.PGInt4 Int where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGInt4 Int32 where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGInt8 Int64 where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGText String where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGFloat8 Double where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGBool Bool where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGUuid UUID where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGBytea SBS.ByteString where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGBytea LBS.ByteString where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGText ST.Text where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGText LT.Text where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGDate Time.Day where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGTimestamptz Time.UTCTime where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGTimestamp Time.LocalTime where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGTime Time.TimeOfDay where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGCitext (CI.CI ST.Text) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGCitext (CI.CI LT.Text) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGJson String where
  queryRunnerColumnDefault = fieldParserQueryRunnerColumn jsonFieldParser

instance QueryRunnerColumnDefault T.PGJson Ae.Value where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGJsonb String where
  queryRunnerColumnDefault = fieldParserQueryRunnerColumn jsonbFieldParser

instance QueryRunnerColumnDefault T.PGJsonb Ae.Value where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

arrayColumn :: Column (T.PGArray a) -> Column a
arrayColumn = C.unsafeCoerceColumn

instance (Typeable b, QueryRunnerColumnDefault a b) =>
         QueryRunnerColumnDefault (T.PGArray a) [b] where
  queryRunnerColumnDefault = QueryRunnerColumn (P.lmap arrayColumn c) ((fmap . fmap . fmap) fromPGArray (arrayFieldParser f))
    where QueryRunnerColumn c f = queryRunnerColumnDefault

-- }

instance (Typeable b, FromField b, QueryRunnerColumnDefault a b) =>
         QueryRunnerColumnDefault (T.PGRange a) (PGSR.PGRange b) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- Boilerplate instances

instance Functor (QueryRunner c) where
  fmap f (QueryRunner u r b) = QueryRunner u ((fmap . fmap) f r) b

-- TODO: Seems like this one should be simpler!
instance Applicative (QueryRunner c) where
  pure = flip (QueryRunner (P.lmap (const ()) PP.empty)) (const False)
         . pure
         . pure
  QueryRunner uf rf bf <*> QueryRunner ux rx bx =
    QueryRunner (P.dimap (\x -> (x,x)) (const ()) (uf PP.***! ux)) ((<*>) <$> rf <*> rx) (liftA2 (||) bf bx)

instance P.Profunctor QueryRunner where
  dimap f g (QueryRunner u r b) =
    QueryRunner (P.lmap f u) (P.dimap f (fmap g) r) (P.lmap f b)

instance PP.ProductProfunctor QueryRunner where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor QueryRunner where
  f +++! g = QueryRunner (P.rmap (const ()) (fu PP.+++! gu))
                         (PackMap.eitherFunction fr gr)
                         (either fb gb)
    where QueryRunner fu fr fb = f
          QueryRunner gu gr gb = g

-- }

-- { Annoying postgresql-simple patch.  Delete this when it is merged upstream.

arrayFieldParser :: Typeable a => FieldParser a -> FieldParser (PGArray a)
arrayFieldParser
    fieldParser f mdat = do
        info <- typeInfo f
        case info of
          TI.Array{} ->
              case mdat of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> do
                   case parseOnly (fromArray fieldParser info f) dat of
                     Left  err  -> returnError ConversionFailed f err
                     Right conv -> PGArray <$> conv
          _ -> returnError Incompatible f ""

fromArray :: FieldParser a -> TypeInfo -> Field -> Parser (Conversion [a])
fromArray fieldParser tInfo f = sequence . (parseIt <$>) <$> array delim
  where
    delim = typdelim (typelem tInfo)
    fElem = f{ typeOid = typoid (typelem tInfo) }

    parseIt item =
        fieldParser f' $ if item' == fromString "NULL" then Nothing else Just item'
      where
        item' = fmt delim item
        f' | Arrays.Array _ <- item = f
           | otherwise              = fElem

-- }

-- { Allow @postgresql-simple@ conversions from JSON types to 'String'

jsonFieldParser, jsonbFieldParser :: FieldParser String
jsonFieldParser  = jsonFieldTypeParser (String.fromString "json")
jsonbFieldParser = jsonFieldTypeParser (String.fromString "jsonb")

-- typenames, not type Oids are used in order to avoid creating
-- a dependency on 'Database.PostgreSQL.LibPQ'
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

prepareRowParser :: QueryRunner columns haskells -> columns -> RowParser haskells
prepareRowParser (QueryRunner _ rowParser nonZeroColumns) cols =
  if nonZeroColumns cols
  then rowParser cols
  else (fromRow :: RowParser (Only Int)) *> rowParser cols
     -- If we are selecting zero columns then the SQL
     -- generator will have to put a dummy 0 into the
     -- SELECT statement, since we can't select zero
     -- columns.  In that case we have to make sure we
     -- read a single Int.
