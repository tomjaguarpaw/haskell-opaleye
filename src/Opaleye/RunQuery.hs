{-# LANGUAGE FlexibleContexts #-}

-- | This module will be deprecated in 0.7.  Use "Opaleye.RunSelect" instead.

module Opaleye.RunQuery (module Opaleye.RunQuery,
                         -- * Datatypes
                         IRQ.Cursor,
                         IRQ.FromFields,
                         IRQ.FromField,
                         QueryRunner,
                         IRQ.QueryRunnerColumn,
                         IRQ.QueryRunnerColumnDefault (..),
                         -- * Creating new 'QueryRunnerColumn's
                         IRQ.fieldQueryRunnerColumn,
                         IRQ.fieldParserQueryRunnerColumn) where

import           Control.Applicative (pure, (<$>))
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Cursor  as PGSC
import qualified Database.PostgreSQL.Simple.FromRow as FR
import qualified Data.String as String

import           Opaleye.Column (Column)
import qualified Opaleye.Select as S
import qualified Opaleye.Sql as S
import qualified Opaleye.Internal.PackMap as PM
import           Opaleye.Internal.RunQuery (QueryRunner(QueryRunner))
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Unpackspec as U

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

-- * Running 'S.Select's

-- | Use 'Opaleye.RunSelect.runSelect' instead.  @runQuery@ will be
-- deprecated in 0.7.
runQuery :: D.Default IRQ.FromFields fields haskells
         => PGS.Connection
         -> S.Select fields
         -> IO [haskells]
runQuery = runQueryExplicit D.def

-- | Use 'Opaleye.RunSelect.runSelectFold' instead.  @runQueryFold@
-- will be deprecated in 0.7.
runQueryFold
  :: D.Default IRQ.FromFields fields haskells
  => PGS.Connection
  -> S.Select fields
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFold = runQueryFoldExplicit D.def

-- * Creating new 'QueryRunnerColumn's

-- | Use 'Opaleye.RunSelect.unsafeFromField' instead.
-- @queryRunnerColumn@ will be deprecated in 0.7.
queryRunnerColumn :: (Column a' -> Column a) -> (b -> b')
                  -> IRQ.FromField a b -> IRQ.FromField a' b'
queryRunnerColumn colF haskellF qrc = IRQ.QueryRunnerColumn (P.lmap colF u)
                                                            (fmapFP haskellF fp)
  where IRQ.QueryRunnerColumn u fp = qrc
        fmapFP = fmap . fmap . fmap

-- * Explicit versions

-- | Use 'Opaleye.RunSelect.runSelectExplict' instead.  Will be
-- deprecated in 0.7.
runQueryExplicit :: IRQ.FromFields fields haskells
                 -> PGS.Connection
                 -> S.Select fields
                 -> IO [haskells]
runQueryExplicit qr conn q = maybe (return []) (PGS.queryWith_ parser conn) sql
  where (sql, parser) = prepareQuery qr q

-- | Use 'Opaleye.RunSelect.runSelectFoldExplict' instead.  Will be
-- deprecated in 0.7.
runQueryFoldExplicit
  :: IRQ.FromFields fields haskells
  -> PGS.Connection
  -> S.Select fields
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFoldExplicit qr conn q z f = case sql of
  Nothing   -> return z
  Just sql' -> PGS.foldWith_ parser conn sql' z f
  where (sql, parser) = prepareQuery qr q

-- * Cursor interface

-- | Use 'Opaleye.RunSelect.declareCursor' instead.  Will be
-- deprecated in 0.7.
declareCursor
    :: D.Default IRQ.FromFields fields haskells
    => PGS.Connection
    -> S.Select fields
    -> IO (IRQ.Cursor haskells)
declareCursor = declareCursorExplicit D.def

-- | Use 'Opaleye.RunSelect.declareCursorExplicit' instead.  Will be
-- deprecated in 0.7.
declareCursorExplicit
    :: IRQ.FromFields fields haskells
    -> PGS.Connection
    -> S.Select fields
    -> IO (IRQ.Cursor haskells)
declareCursorExplicit qr conn q =
    case mbQuery of
      Nothing    -> pure IRQ.EmptyCursor
      Just query -> IRQ.Cursor rowParser <$> PGSC.declareCursor conn query
  where
    (mbQuery, rowParser) = prepareQuery qr q

-- | Use 'Opaleye.RunSelect.closeCursor' instead.  Will be
-- deprecated in 0.7.
closeCursor :: IRQ.Cursor fields -> IO ()
closeCursor IRQ.EmptyCursor       = pure ()
closeCursor (IRQ.Cursor _ cursor) = PGSC.closeCursor cursor

-- | Use 'Opaleye.RunSelect.foldForward' instead.  Will be
-- deprecated in 0.7.
foldForward
    :: IRQ.Cursor haskells
    -> Int
    -> (a -> haskells -> IO a)
    -> a
    -> IO (Either a a)
foldForward IRQ.EmptyCursor              _chunkSize _f z = pure $ Left z
foldForward (IRQ.Cursor rowParser cursor) chunkSize  f z =
    PGSC.foldForwardWithParser cursor rowParser chunkSize f z

-- * Deprecated functions

{-# DEPRECATED prepareQuery "Will be removed in version 0.7" #-}
prepareQuery :: IRQ.FromFields fields haskells -> S.Select fields -> (Maybe PGS.Query, FR.RowParser haskells)
prepareQuery qr@(QueryRunner u _ _) q = (sql, parser)
  where sql :: Maybe PGS.Query
        sql = fmap String.fromString (S.showSqlForPostgresExplicit u q)
        -- FIXME: We're doing work twice here
        (b, _, _) = Q.runSimpleQueryArrStart q ()
        parser = IRQ.prepareRowParser qr b

-- | Naughty and dangerous, but sometimes fun
app :: QueryRunner (columns, QueryRunner columns haskells) haskells
app = QueryRunner
      (U.Unpackspec (PM.PackMap (\traverseExprs (columns, qr) -> case u qr of
                                    U.Unpackspec (PM.PackMap uqr) ->
                                      uqr traverseExprs columns)))
      (\(columns, qr) -> r qr columns)
      (\(columns, qr) -> b qr columns)
  where u (QueryRunner u' _ _) = u'
        r (QueryRunner _ r' _) = r'
        b (QueryRunner _ _ b') = b'
