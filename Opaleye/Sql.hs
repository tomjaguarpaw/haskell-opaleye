{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Opaleye.Sql where

import qualified Database.HaskellDB.PrimQuery as HPQ
import qualified Database.HaskellDB.Sql.Print as P

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.QueryArr as Q

import qualified Data.Profunctor.Product.Default as D

import           Karamaan.Plankton ((.:))

showSqlForPostgres :: forall columns . D.Default U.Unpackspec columns columns =>
                      Q.Query columns -> String
showSqlForPostgres = showSqlForPostgresExplicit (D.def :: U.Unpackspec columns columns)

showSqlForPostgresUnopt :: forall columns . D.Default U.Unpackspec columns columns =>
                           Q.Query columns -> String
showSqlForPostgresUnopt = showSqlForPostgresUnoptExplicit (D.def :: U.Unpackspec columns columns)

showSqlForPostgresExplicit :: U.Unpackspec columns b -> Q.Query columns -> String
--showSqlForPostgresExplicit = formatAndShowSQL . O.optimize .: Q.runQueryArrUnpack
showSqlForPostgresExplicit = formatAndShowSQL .: Q.runQueryArrUnpack

showSqlForPostgresUnoptExplicit :: U.Unpackspec columns b -> Q.Query columns -> String
--showSqlForPostgresUnoptExplicit = formatAndShowSQL .: Q.runQueryArrUnpack
showSqlForPostgresUnoptExplicit = error "showSqlForPostgresUnoptExplicit not implemented"

formatAndShowSQL :: (PQ.PrimQuery, [HPQ.PrimExpr]) -> String
formatAndShowSQL = show . P.ppSql . Sql.sql
