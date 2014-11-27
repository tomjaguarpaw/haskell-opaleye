{-# LANGUAGE FlexibleContexts #-}

module Opaleye.RunQuery (module Opaleye.RunQuery, QueryRunner) where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.String as String

import qualified Opaleye.Sql as S
import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.RunQuery (QueryRunner(QueryRunner))

import qualified Data.Profunctor.Product.Default as D

runQuery :: D.Default QueryRunner columns haskells
         => PGS.Connection
         -> Query columns
         -> IO [haskells]
runQuery = runQueryExplicit D.def

runQueryExplicit :: QueryRunner columns haskells
                 -> PGS.Connection
                 -> Query columns
                 -> IO [haskells]
runQueryExplicit (QueryRunner u rowParser) conn q =
  PGS.queryWith_ rowParser conn sql
  where sql :: PGS.Query
        sql = String.fromString (S.showSqlForPostgresExplicit u q)
