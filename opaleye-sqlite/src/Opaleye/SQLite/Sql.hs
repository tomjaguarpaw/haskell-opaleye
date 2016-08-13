{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Opaleye.SQLite.Sql where

import qualified Opaleye.SQLite.Internal.HaskellDB.PrimQuery as HPQ

import qualified Opaleye.SQLite.Internal.Unpackspec as U
import qualified Opaleye.SQLite.Internal.Sql as Sql
import qualified Opaleye.SQLite.Internal.Print as Pr
import qualified Opaleye.SQLite.Internal.PrimQuery as PQ
import qualified Opaleye.SQLite.Internal.Optimize as Op
import           Opaleye.SQLite.Internal.Helpers ((.:))
import qualified Opaleye.SQLite.Internal.QueryArr as Q
import qualified Opaleye.SQLite.Internal.Tag as T

import qualified Data.Profunctor.Product.Default as D

-- | Example type specialization:
--
-- @
-- showSqlForPostgres :: Query (Column a, Column b) -> String
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- showSqlForPostgres :: Query (Foo (Column a) (Column b) (Column c)) -> String
-- @
showSqlForPostgres :: forall columns . D.Default U.Unpackspec columns columns =>
                      Q.Query columns -> String
showSqlForPostgres = showSqlForPostgresExplicit (D.def :: U.Unpackspec columns columns)

showSqlForPostgresUnopt :: forall columns . D.Default U.Unpackspec columns columns =>
                           Q.Query columns -> String
showSqlForPostgresUnopt = showSqlForPostgresUnoptExplicit (D.def :: U.Unpackspec columns columns)

showSqlForPostgresExplicit :: U.Unpackspec columns b -> Q.Query columns -> String
showSqlForPostgresExplicit = formatAndShowSQL
                             . (\(x, y, z) -> (x, Op.optimize y, z))
                             .: Q.runQueryArrUnpack

showSqlForPostgresUnoptExplicit :: U.Unpackspec columns b -> Q.Query columns -> String
showSqlForPostgresUnoptExplicit = formatAndShowSQL .: Q.runQueryArrUnpack

formatAndShowSQL :: ([HPQ.PrimExpr], PQ.PrimQuery, T.Tag) -> String
formatAndShowSQL = show . Pr.ppSql . Sql.sql
