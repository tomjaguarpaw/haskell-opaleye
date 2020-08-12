{-# LANGUAGE FlexibleContexts #-}

module Opaleye.RunQuery {-# DEPRECATED "Use \"Opaleye.RunSelect\" instead." #-}
                 (module Opaleye.RunQuery,
                         -- * Datatypes
                         IRQ.Cursor,
                         IRQ.FromFields,
                         IRQ.FromField,
                         QueryRunner,
                         IRQ.QueryRunnerColumn,
                         IRQ.QueryRunnerColumnDefault,
                         -- * Creating new 'QueryRunnerColumn's
                         IRQ.fieldQueryRunnerColumn,
                         IRQ.fieldParserQueryRunnerColumn) where

import           Control.Applicative (pure, (<$>))
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Cursor  as PGSC

import           Opaleye.Column (Column)
import qualified Opaleye.Select as S
import           Opaleye.Internal.RunQuery (QueryRunner, prepareQuery)
import qualified Opaleye.Internal.RunQuery as IRQ

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

-- * Running 'S.Select's

{-# DEPRECATED runQuery "Use 'Opaleye.RunSelect.runSelect' instead.  @runQuery@ will be removed in 0.8." #-}
runQuery :: D.Default IRQ.FromFields fields haskells
         => PGS.Connection
         -> S.Select fields
         -> IO [haskells]
runQuery = runQueryExplicit D.def

{-# DEPRECATED runQueryFold "Use 'Opaleye.RunSelect.runSelectFold' instead.  @runQueryFold@ will be removed in 0.8." #-}
runQueryFold
  :: D.Default IRQ.FromFields fields haskells
  => PGS.Connection
  -> S.Select fields
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFold = runQueryFoldExplicit D.def

-- * Creating new 'QueryRunnerColumn's

{-# DEPRECATED queryRunnerColumn "Use 'Opaleye.RunSelect.unsafeFromField' instead. @queryRunnerColumn@ will be removed in 0.8." #-}
queryRunnerColumn :: (Column a' -> Column a) -> (b -> b')
                  -> IRQ.FromField a b -> IRQ.FromField a' b'
queryRunnerColumn colF haskellF qrc = IRQ.QueryRunnerColumn (P.lmap colF u)
                                                            (fmapFP haskellF fp)
  where IRQ.QueryRunnerColumn u fp = qrc
        fmapFP = fmap . fmap . fmap

-- * Explicit versions

{-# DEPRECATED runQueryExplicit "Use 'Opaleye.RunSelect.runSelectExplict' instead.  Will be removed in 0.8." #-}
runQueryExplicit :: IRQ.FromFields fields haskells
                 -> PGS.Connection
                 -> S.Select fields
                 -> IO [haskells]
runQueryExplicit qr conn q = maybe (return []) (PGS.queryWith_ parser conn) sql
  where (sql, parser) = IRQ.prepareQuery qr q

{-# DEPRECATED runQueryFoldExplicit "Use 'Opaleye.RunSelect.runSelectFoldExplict' instead.  Will be deprecated in 0.8." #-}
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

{-# DEPRECATED declareCursor "Use 'Opaleye.RunSelect.declareCursor' instead.  Will be removed in 0.8." #-}
declareCursor
    :: D.Default IRQ.FromFields fields haskells
    => PGS.Connection
    -> S.Select fields
    -> IO (IRQ.Cursor haskells)
declareCursor = declareCursorExplicit D.def

{-# DEPRECATED declareCursorExplicit "Use 'Opaleye.RunSelect.declareCursorExplicit' instead.  Will be removed in 0.8." #-}
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

{-# DEPRECATED closeCursor "Use 'Opaleye.RunSelect.closeCursor' instead.  Will be removed in 0.8." #-}
closeCursor :: IRQ.Cursor fields -> IO ()
closeCursor IRQ.EmptyCursor       = pure ()
closeCursor (IRQ.Cursor _ cursor) = PGSC.closeCursor cursor

{-# DEPRECATED foldForward "Use 'Opaleye.RunSelect.foldForward' instead.  Will be removed in 0.8." #-}
foldForward
    :: IRQ.Cursor haskells
    -> Int
    -> (a -> haskells -> IO a)
    -> a
    -> IO (Either a a)
foldForward IRQ.EmptyCursor              _chunkSize _f z = pure $ Left z
foldForward (IRQ.Cursor rowParser cursor) chunkSize  f z =
    PGSC.foldForwardWithParser cursor rowParser chunkSize f z
