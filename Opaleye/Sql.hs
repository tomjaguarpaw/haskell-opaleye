{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Opaleye.Sql where

import qualified Database.HaskellDB.PrimQuery as HPQ

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.SqlOpaleye as Sql
import qualified Opaleye.Internal.Print as Pr
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Optimize as Op
import qualified Opaleye.QueryArr as Q

import qualified Data.Profunctor.Product.Default as D

import qualified Control.Arrow as Arr

import           Karamaan.Plankton ((.:))

showSqlForPostgres :: forall columns . D.Default U.Unpackspec columns columns =>
                      Q.Query columns -> String
showSqlForPostgres = showSqlForPostgresExplicit (D.def :: U.Unpackspec columns columns)

showSqlForPostgresUnopt :: forall columns . D.Default U.Unpackspec columns columns =>
                           Q.Query columns -> String
showSqlForPostgresUnopt = showSqlForPostgresUnoptExplicit (D.def :: U.Unpackspec columns columns)

showSqlForPostgresExplicit :: U.Unpackspec columns b -> Q.Query columns -> String
showSqlForPostgresExplicit = formatAndShowSQL
                             . Arr.first Op.optimize
                             .: Q.runQueryArrUnpack

showSqlForPostgresUnoptExplicit :: U.Unpackspec columns b -> Q.Query columns -> String
showSqlForPostgresUnoptExplicit = formatAndShowSQL .: Q.runQueryArrUnpack

formatAndShowSQL :: (PQ.PrimQuery, [HPQ.PrimExpr]) -> String
formatAndShowSQL = show . Pr.ppSql . Sql.sql
