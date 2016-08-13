{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.SQLite.Internal.RunQuery where

import           Control.Applicative (Applicative, pure, (<$>), (<*>), liftA2)

import           Database.SQLite.Simple.Internal (RowParser)
import           Database.SQLite.Simple.FromField (FieldParser, FromField,
                                                       fromField)
import qualified Database.SQLite.Simple.Internal as SSI
import           Database.SQLite.Simple.FromRow (fieldWith)
import qualified Database.SQLite3 as SQLiteBase

import           Opaleye.SQLite.Column (Column)
import           Opaleye.SQLite.Internal.Column (Nullable)
import qualified Opaleye.SQLite.Internal.PackMap as PackMap
import qualified Opaleye.SQLite.Column as C
import qualified Opaleye.SQLite.Internal.Unpackspec as U
import qualified Opaleye.SQLite.PGTypes as T

import qualified Data.Profunctor as P
import           Data.Profunctor (dimap)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product (empty, (***!))
import qualified Data.Profunctor.Product.Default as D

import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time as Time
import           GHC.Int (Int64)

-- | A 'QueryRunnerColumn' @pgType@ @haskellType@ encodes how to turn
-- a value of Postgres type @pgType@ into a value of Haskell type
-- @haskellType@.  For example a value of type 'QueryRunnerColumn'
-- 'T.PGText' 'String' encodes how to turn a 'PGText' result from the
-- database into a Haskell 'String'.

-- This is *not* a Product Profunctor because it is the only way I
-- know of to get the instance generation to work for non-Nullable and
-- Nullable types at once.
data QueryRunnerColumn sqlType haskellType =
  QueryRunnerColumn (U.Unpackspec (Column sqlType) ()) (FieldParser haskellType)

data QueryRunner columns haskells =
  QueryRunner (U.Unpackspec columns ())
              (columns -> RowParser haskells)
              -- We never actually
              -- look at the columns
              -- except to see its
              -- "type" in the case
              -- of a sum profunctor
              (columns -> Bool)
              -- ^ Have we actually requested any columns?  If we
              -- asked for zero columns then the SQL generator will
              -- have to put a dummy 0 into the SELECT statement,
              -- since we can't select zero columns.  In that case we
              -- have to make sure we read a single Int.

fieldQueryRunnerColumn :: FromField haskell => QueryRunnerColumn coltype haskell
fieldQueryRunnerColumn =
  QueryRunnerColumn (P.rmap (const ()) U.unpackspecColumn) fromField

queryRunner :: QueryRunnerColumn a b -> QueryRunner (Column a) b
queryRunner qrc = QueryRunner u (const (fieldWith fp)) (const True)
    where QueryRunnerColumn u fp = qrc

queryRunnerColumnNullable :: QueryRunnerColumn a b
                       -> QueryRunnerColumn (Nullable a) (Maybe b)
queryRunnerColumnNullable qr =
  QueryRunnerColumn (P.lmap C.unsafeCoerceColumn u) (fromField' fp)
  where QueryRunnerColumn u fp = qr
        fromField' :: FieldParser a -> FieldParser (Maybe a)
        fromField' _ (SSI.Field SQLiteBase.SQLNull _) = pure Nothing
        fromField' fp' f = fmap Just (fp' f)

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
-- Haskell value of type @haskelType@.
class QueryRunnerColumnDefault pgType haskellType where
  queryRunnerColumnDefault :: QueryRunnerColumn pgType haskellType

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
