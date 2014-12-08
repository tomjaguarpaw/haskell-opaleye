{-# LANGUAGE FlexibleContexts #-}

module Opaleye.RunQuery (module Opaleye.RunQuery,
                         QueryRunner,
                         IRQ.QueryRunnerColumn,
                         IRQ.fieldQueryRunnerColumn) where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.String as String

-- It seems that we need the import of unsafeCoerce for Haddock
import           Opaleye.Column (Column, unsafeCoerce)
import qualified Opaleye.Sql as S
import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.RunQuery (QueryRunner(QueryRunner))
import qualified Opaleye.Internal.RunQuery as IRQ

import qualified Data.Profunctor as P
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

-- | Use 'queryRunnerColumn' to make an instance to allow you to run queries on
--   your own datatypes.  For example:
--
-- @
-- newtype Foo = Foo Int
-- instance Default QueryRunnerColumn Foo Foo where
--    def = queryRunnerColumn ('unsafeCoerce' :: Column Foo -> Column Int) Foo def
-- @
queryRunnerColumn :: (Column a' -> Column a) -> (b -> b')
                  -> IRQ.QueryRunnerColumn a b -> IRQ.QueryRunnerColumn a' b'
queryRunnerColumn colF haskellF qrc = IRQ.QueryRunnerColumn (P.lmap colF u)
                                                            (fmapFP haskellF fp)
  where IRQ.QueryRunnerColumn u fp = qrc
        fmapFP = fmap . fmap . fmap
