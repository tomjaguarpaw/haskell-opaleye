{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Opaleye.Sql where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Print as Pr
import qualified Opaleye.Internal.Optimize as Op
import           Opaleye.Internal.Helpers ((.:))
import qualified Opaleye.Internal.QueryArr as Q

import qualified Opaleye.Select as S

import qualified Data.Profunctor.Product.Default as D

-- * Showing SQL

-- | Show the SQL query string generated from the 'S.Select'.
--
-- When 'Nothing' is returned it means that the 'S.Select' returns zero
-- rows.
--
-- Example type specialization:
--
-- @
-- showSql :: Select (Field a, Field b) -> Maybe String
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- showSql :: Select (Foo (Field a) (Field b) (Field c)) -> Maybe String
-- @
showSql :: forall fields.
           D.Default U.Unpackspec fields fields
        => S.Select fields
        -> Maybe String
showSql = showSqlExplicit (D.def :: U.Unpackspec fields fields)

-- | Show the unoptimized SQL query string generated from the 'S.Select'.
showSqlUnopt :: forall fields.
                D.Default U.Unpackspec fields fields
             => S.Select fields
             -> Maybe String
showSqlUnopt = showSqlUnoptExplicit (D.def :: U.Unpackspec fields fields)

-- * Explicit versions

showSqlExplicit :: U.Unpackspec fields b -> S.Select fields -> Maybe String
showSqlExplicit = Pr.formatAndShowSQL
                  . (\(x, y, z) -> (x, Op.optimize y, z))
                  .: Q.runQueryArrUnpack

showSqlUnoptExplicit :: U.Unpackspec fields b -> S.Select fields -> Maybe String
showSqlUnoptExplicit = Pr.formatAndShowSQL .: Q.runQueryArrUnpack

-- * Deprecated functions

{-# DEPRECATED showSqlForPostgres "Will be removed in version 0.8.  Use 'showSql' instead." #-}
showSqlForPostgres :: forall columns . D.Default U.Unpackspec columns columns =>
                      S.Select columns -> Maybe String
showSqlForPostgres = showSql

{-# DEPRECATED showSqlForPostgresUnopt "Will be removed in version 0.8.  Use 'showSqlUnopt' instead." #-}
showSqlForPostgresUnopt :: forall columns . D.Default U.Unpackspec columns columns =>
                           S.Select columns -> Maybe String
showSqlForPostgresUnopt = showSqlUnopt

{-# DEPRECATED showSqlForPostgresExplicit "Will be removed in version 0.8.  Use 'showSqlExplicit' instead." #-}
showSqlForPostgresExplicit :: U.Unpackspec columns b -> S.Select columns -> Maybe String
showSqlForPostgresExplicit = showSqlExplicit

{-# DEPRECATED showSqlForPostgresUnoptExplicit "Will be removed in version 0.8.  Use 'showSqlUnoptExplicit' instead." #-}
showSqlForPostgresUnoptExplicit :: U.Unpackspec columns b -> S.Select columns -> Maybe String
showSqlForPostgresUnoptExplicit = showSqlUnoptExplicit
