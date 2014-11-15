{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.RunQuery where

import           Database.PostgreSQL.Simple.Internal (RowParser)
import           Database.PostgreSQL.Simple.FromField (FieldParser, FromField,
                                                       fromField)
import           Database.PostgreSQL.Simple.FromRow (fieldWith)

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.String as S
import           Control.Applicative (Applicative, pure, (<*>))

import           Opaleye.Column (Column, Nullable)
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Sql as S
import           Opaleye.QueryArr (Query)

import qualified Data.Profunctor as P
import           Data.Profunctor (dimap)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product (empty, (***!))
import qualified Data.Profunctor.Product.Default as D

data QueryRunner columns haskells = QueryRunner (U.Unpackspec columns ())
                                                (RowParser haskells)

runQueryExplicit :: QueryRunner columns haskells
                 -> SQL.Connection
                 -> Query columns
                 -> IO [haskells]
runQueryExplicit (QueryRunner u rowParser) conn q =
  SQL.queryWith_ rowParser conn sql
  where sql :: SQL.Query
        sql = S.fromString (S.showSqlForPostgresExplicit u q)

runQuery :: D.Default QueryRunner columns haskells
         => SQL.Connection
         -> Query columns
         -> IO [haskells]
runQuery = runQueryExplicit D.def

fieldQueryRunner :: FromField a => QueryRunner (Column a) a
fieldQueryRunner = fieldQueryRunnerF id

fieldQueryRunnerF :: FromField a => (a -> b) -> QueryRunner (Column b) b
fieldQueryRunnerF = fieldQueryRunnerUnclassed . flip fmapFieldParser fromField

fmapFieldParser :: (a -> b) -> FieldParser a -> FieldParser b
fmapFieldParser = fmap . fmap . fmap

-- TODO: May want to make this "(Column b) a" in the future
fieldQueryRunnerUnclassed :: FieldParser a -> QueryRunner (Column a) a
fieldQueryRunnerUnclassed = QueryRunner (P.rmap (const ()) U.unpackspecColumn)
                            . fieldWith

instance D.Default QueryRunner (Column Int) Int where
  def = fieldQueryRunner

instance D.Default QueryRunner (Column Integer) Integer where
  def = fieldQueryRunner

instance D.Default QueryRunner (Column Double) Double where
  def = fieldQueryRunner

-- The unsafeCoerce is a bit silly here
instance D.Default QueryRunner (Column (Nullable Int)) (Maybe Int) where
  def = P.lmap C.unsafeCoerce fieldQueryRunner

-- {

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
