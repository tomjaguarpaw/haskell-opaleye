{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.RunQuery where

import           Control.Applicative (Applicative, pure, (<*>))

import           Database.PostgreSQL.Simple.Internal (RowParser)
import           Database.PostgreSQL.Simple.FromField (FieldParser, FromField,
                                                       fromField)
import           Database.PostgreSQL.Simple.FromRow (fieldWith)
import           Database.PostgreSQL.Simple.Types (fromPGArray)

import           Opaleye.Column (Column)
import           Opaleye.Internal.Column (Nullable)
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.PGTypes as T

import qualified Data.Profunctor as P
import           Data.Profunctor (dimap)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product (empty, (***!))
import qualified Data.Profunctor.Product.Default as D

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import           Data.UUID (UUID)
import           GHC.Int (Int64)

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
import           Data.String (fromString)
import           Data.Typeable (Typeable)

-- }

-- We introduce 'QueryRunnerColumn' which is *not* a Product
-- Profunctor because it is the only way I know of to get the instance
-- generation to work for non-Nullable and Nullable types at once.
data QueryRunnerColumn coltype haskell =
  QueryRunnerColumn (U.Unpackspec (Column coltype) ()) (FieldParser haskell)

data QueryRunner columns haskells = QueryRunner (U.Unpackspec columns ())
                                                (RowParser haskells)

fieldQueryRunnerColumn :: FromField haskell => QueryRunnerColumn coltype haskell
fieldQueryRunnerColumn =
  QueryRunnerColumn (P.rmap (const ()) U.unpackspecColumn) fromField

queryRunner :: QueryRunnerColumn a b -> QueryRunner (Column a) b
queryRunner qrc = QueryRunner u (fieldWith fp)
    where QueryRunnerColumn u fp = qrc

queryRunnerColumnNullable :: QueryRunnerColumn a b
                       -> QueryRunnerColumn (Nullable a) (Maybe b)
queryRunnerColumnNullable qr =
  QueryRunnerColumn (P.lmap C.unsafeCoerce u) (fromField' fp)
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

class QueryRunnerColumnDefault a b where
  queryRunnerColumnDefault :: QueryRunnerColumn a b

instance QueryRunnerColumnDefault T.PGInt4 Int where
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

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

arrayColumn :: Column (T.PGArray a) -> Column a
arrayColumn = C.unsafeCoerce

instance (Typeable b, QueryRunnerColumnDefault a b) =>
         QueryRunnerColumnDefault (T.PGArray a) [b] where
  queryRunnerColumnDefault = QueryRunnerColumn (P.lmap arrayColumn c) ((fmap . fmap . fmap) fromPGArray (arrayFieldParser f))
    where QueryRunnerColumn c f = queryRunnerColumnDefault

-- }

-- Boilerplate instances

instance Functor (QueryRunner c) where
  fmap f (QueryRunner u r) = QueryRunner u (fmap f r)

-- TODO: Seems like this one should be simpler!
instance Applicative (QueryRunner c) where
  pure = QueryRunner (P.lmap (const ()) PP.empty) . pure
  QueryRunner uf rf <*> QueryRunner ux rx =
    QueryRunner (P.dimap (\x -> (x,x)) (const ()) (uf PP.***! ux)) (rf <*> rx)

instance P.Profunctor QueryRunner where
  dimap f g (QueryRunner u r) = QueryRunner (P.lmap f u) (fmap g r)

instance PP.ProductProfunctor QueryRunner where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

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
fromArray fieldParser typeInfo f = sequence . (parseIt <$>) <$> array delim
  where
    delim = typdelim (typelem typeInfo)
    fElem = f{ typeOid = typoid (typelem typeInfo) }

    parseIt item =
        fieldParser f' $ if item' == fromString "NULL" then Nothing else Just item'
      where
        item' = fmt delim item
        f' | Arrays.Array _ <- item = f
           | otherwise              = fElem

-- }
