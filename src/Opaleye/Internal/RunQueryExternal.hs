{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Internal.RunQueryExternal
                 (module Opaleye.Internal.RunQueryExternal,
                         -- * Datatypes
                         IRQ.Cursor,
                         IRQ.FromFields,
                         IRQ.FromField,
                         -- * Creating new 'FromField's
                         IRQ.fieldQueryRunnerColumn,
                         IRQ.fieldParserQueryRunnerColumn) where

import           Control.Applicative (pure, (<$>))
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Cursor  as PGSC

import           Opaleye.Column (Column)
import qualified Opaleye.Select as S
import           Opaleye.Internal.RunQuery (prepareQuery)
import qualified Opaleye.Internal.RunQuery as IRQ

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

-- * Running 'S.Select's

runQuery :: D.Default IRQ.FromFields fields haskells
         => PGS.Connection
         -> S.Select fields
         -> IO [haskells]
runQuery = runQueryExplicit D.def

runQueryFold
  :: D.Default IRQ.FromFields fields haskells
  => PGS.Connection
  -> S.Select fields
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFold = runQueryFoldExplicit D.def

-- * Explicit versions

runQueryExplicit :: IRQ.FromFields fields haskells
                 -> PGS.Connection
                 -> S.Select fields
                 -> IO [haskells]
runQueryExplicit qr conn q = maybe (return []) (PGS.queryWith_ parser conn) sql
  where (sql, parser) = IRQ.prepareQuery qr q

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

declareCursor
    :: D.Default IRQ.FromFields fields haskells
    => PGS.Connection
    -> S.Select fields
    -> IO (IRQ.Cursor haskells)
declareCursor = declareCursorExplicit D.def

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

closeCursor :: IRQ.Cursor fields -> IO ()
closeCursor IRQ.EmptyCursor       = pure ()
closeCursor (IRQ.Cursor _ cursor) = PGSC.closeCursor cursor

foldForward
    :: IRQ.Cursor haskells
    -> Int
    -> (a -> haskells -> IO a)
    -> a
    -> IO (Either a a)
foldForward IRQ.EmptyCursor              _chunkSize _f z = pure $ Left z
foldForward (IRQ.Cursor rowParser cursor) chunkSize  f z =
    PGSC.foldForwardWithParser cursor rowParser chunkSize f z
