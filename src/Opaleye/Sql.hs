{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Opaleye.Sql where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Internal.Print as Pr
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Optimize as Op
import           Opaleye.Internal.Helpers ((.:))
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Tag as T

import qualified Data.Profunctor.Product.Default as D

-- | Show the SQL query string generated from the query.
--
-- When 'Nothing' is returned it means that the 'Query' returns zero
-- rows.
--
-- Example type specialization:
--
-- @
-- showSql :: Query (Column a, Column b) -> Maybe String
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- showSql :: Query (Foo (Column a) (Column b) (Column c)) -> Maybe String
-- @
showSql :: forall columns.
           D.Default U.Unpackspec columns columns
        => Q.Query columns
        -> Maybe String
showSql = showSqlExplicit (D.def :: U.Unpackspec columns columns)

-- | Show the unoptimized SQL query string generated from the query.
showSqlUnopt :: forall columns.
                D.Default U.Unpackspec columns columns
             => Q.Query columns
             -> Maybe String
showSqlUnopt = showSqlUnoptExplicit (D.def :: U.Unpackspec columns columns)

-- * Explicit versions

showSqlExplicit :: U.Unpackspec columns b -> Q.Query columns -> Maybe String
showSqlExplicit = formatAndShowSQL
                  . (\(x, y, z) -> (x, Op.optimize y, z))
                  .: Q.runQueryArrUnpack

showSqlUnoptExplicit :: U.Unpackspec columns b -> Q.Query columns -> Maybe String
showSqlUnoptExplicit = formatAndShowSQL .: Q.runQueryArrUnpack

-- * Deprecated functions

-- | Will be deprecated in version 0.7.  Use 'showSql' instead.
showSqlForPostgres :: forall columns . D.Default U.Unpackspec columns columns =>
                      Q.Query columns -> Maybe String
showSqlForPostgres = showSql

-- | Will be deprecated in version 0.7.  Use 'showSqlUnopt' instead.
showSqlForPostgresUnopt :: forall columns . D.Default U.Unpackspec columns columns =>
                           Q.Query columns -> Maybe String
showSqlForPostgresUnopt = showSqlUnopt

-- | Will be deprecated in version 0.7.  Use 'showSqlExplicit' instead.
showSqlForPostgresExplicit :: U.Unpackspec columns b -> Q.Query columns -> Maybe String
showSqlForPostgresExplicit = showSqlExplicit

-- | Will be deprecated in version 0.7.  Use 'showSqlUnoptExplicit' instead.
showSqlForPostgresUnoptExplicit :: U.Unpackspec columns b -> Q.Query columns -> Maybe String
showSqlForPostgresUnoptExplicit = showSqlUnoptExplicit

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
formatAndShowSQL :: ([HPQ.PrimExpr], PQ.PrimQuery' a, T.Tag) -> Maybe String
formatAndShowSQL = fmap (show . Pr.ppSql . Sql.sql) . traverse2Of3 Op.removeEmpty
  where -- Just a lens
        traverse2Of3 :: Functor f => (a -> f b) -> (x, a, y) -> f (x, b, y)
        traverse2Of3 f (x, y, z) = fmap (\y' -> (x, y', z)) (f y)
