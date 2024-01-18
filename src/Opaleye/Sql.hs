{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Sql (
  -- * Showing SQL
  showSql,
  showSqlUnopt,
  -- * Explicit versions
  showSqlExplicit,
  showSqlUnoptExplicit,
  ) where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Print as Pr
import qualified Opaleye.Internal.Optimize as Op
import           Opaleye.Internal.Helpers ((.:), atSameType)
import qualified Opaleye.Internal.QueryArr as Q

import qualified Opaleye.Select as S

import qualified Data.Profunctor.Product.Default as D

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
showSql :: D.Default U.Unpackspec fields fields
        => S.Select fields
        -> Maybe String
showSql = showSqlExplicit (atSameType D.def)

-- | Show the unoptimized SQL query string generated from the 'S.Select'.
showSqlUnopt :: D.Default U.Unpackspec fields fields
             => S.Select fields
             -> Maybe String
showSqlUnopt = showSqlUnoptExplicit (atSameType D.def)

showSqlExplicit :: U.Unpackspec fields b -> S.Select fields -> Maybe String
showSqlExplicit = Pr.formatAndShowSQL
                  . (\(x, y, z) -> (x, Op.optimize y, z))
                  .: Q.runQueryArrUnpack

showSqlUnoptExplicit :: U.Unpackspec fields b -> S.Select fields -> Maybe String
showSqlUnoptExplicit = Pr.formatAndShowSQL .: Q.runQueryArrUnpack
