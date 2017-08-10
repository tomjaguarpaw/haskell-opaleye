{-# LANGUAGE FlexibleContexts #-}

module Opaleye.RunQuery (module Opaleye.RunQuery,
                         -- * Datatypes
                         IRQ.Cursor,
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
import qualified Opaleye.Sql as S
import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.RunQuery (QueryRunner(QueryRunner))
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Internal.QueryArr as Q

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

-- * Running 'Query's

-- | @runQuery@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runQuery@.
--
-- Example type specialization:
--
-- @
-- runQuery :: Query (Column 'Opaleye.PGTypes.PGInt4', Column 'Opaleye.PGTypes.PGText') -> IO [(Int, String)]
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- runQuery :: Query (Foo (Column 'Opaleye.PGTypes.PGInt4') (Column 'Opaleye.PGTypes.PGText') (Column 'Opaleye.PGTypes.PGBool')
--          -> IO [Foo Int String Bool]
-- @
--
-- Opaleye types are converted to Haskell types based on instances of
-- the 'Opaleye.Internal.RunQuery.QueryRunnerColumnDefault' typeclass.
runQuery :: D.Default QueryRunner columns haskells
         => PGS.Connection
         -> Query columns
         -> IO [haskells]
runQuery = runQueryExplicit D.def

-- | @runQueryFold@ streams the results of a query incrementally and consumes
-- the results with a left fold.
--
-- This fold is /not/ strict. The stream consumer is responsible for
-- forcing the evaluation of its result to avoid space leaks.
runQueryFold
  :: D.Default QueryRunner columns haskells
  => PGS.Connection
  -> Query columns
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFold = runQueryFoldExplicit D.def

-- * Creating new 'QueryRunnerColumn's

-- | Use 'queryRunnerColumn' to make an instance to allow you to run queries on
--   your own datatypes.  For example:
--
-- @
-- newtype Foo = Foo Int
--
-- instance QueryRunnerColumnDefault Foo Foo where
--    queryRunnerColumnDefault =
--        queryRunnerColumn ('Opaleye.Column.unsafeCoerceColumn'
--                               :: Column Foo -> Column PGInt4)
--                          Foo
--                          queryRunnerColumnDefault
-- @
queryRunnerColumn :: (Column a' -> Column a) -> (b -> b')
                  -> IRQ.QueryRunnerColumn a b -> IRQ.QueryRunnerColumn a' b'
queryRunnerColumn colF haskellF qrc = IRQ.QueryRunnerColumn (P.lmap colF u)
                                                            (fmapFP haskellF fp)
  where IRQ.QueryRunnerColumn u fp = qrc
        fmapFP = fmap . fmap . fmap

-- * Explicit versions

runQueryExplicit :: QueryRunner columns haskells
                 -> PGS.Connection
                 -> Query columns
                 -> IO [haskells]
runQueryExplicit qr conn q = maybe (return []) (PGS.queryWith_ parser conn) sql
  where (sql, parser) = prepareQuery qr q

runQueryFoldExplicit
  :: QueryRunner columns haskells
  -> PGS.Connection
  -> Query columns
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFoldExplicit qr conn q z f = case sql of
  Nothing   -> return z
  Just sql' -> PGS.foldWith_ parser conn sql' z f
  where (sql, parser) = prepareQuery qr q

-- * Cursor interface

-- | Declare a temporary cursor. The cursor is given a unique name for the given
-- connection.
--
-- Returns 'Nothing' when the query returns zero rows.
declareCursor
    :: D.Default QueryRunner columns haskells
    => PGS.Connection
    -> Query columns
    -> IO (IRQ.Cursor haskells)
declareCursor = declareCursorExplicit D.def

-- | Like 'declareCursor' but takes a 'QueryRunner' explicitly.
declareCursorExplicit
    :: QueryRunner columns haskells
    -> PGS.Connection
    -> Query columns
    -> IO (IRQ.Cursor haskells)
declareCursorExplicit qr conn q =
    case mbQuery of
      Nothing    -> pure IRQ.EmptyCursor
      Just query -> IRQ.Cursor rowParser <$> PGSC.declareCursor conn query
  where
    (mbQuery, rowParser) = prepareQuery qr q

-- | Close the given cursor.
closeCursor :: IRQ.Cursor columns -> IO ()
closeCursor IRQ.EmptyCursor       = pure ()
closeCursor (IRQ.Cursor _ cursor) = PGSC.closeCursor cursor

-- | Fold over a chunk of rows, calling the supplied fold-like function on each
-- row as it is received. In case the cursor is exhausted, a 'Left' value is
-- returned, otherwise a 'Right' value is returned.
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

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
prepareQuery :: QueryRunner columns haskells -> Query columns -> (Maybe PGS.Query, FR.RowParser haskells)
prepareQuery qr@(QueryRunner u _ _) q = (sql, parser)
  where sql :: Maybe PGS.Query
        sql = fmap String.fromString (S.showSqlForPostgresExplicit u q)
        -- FIXME: We're doing work twice here
        (b, _, _) = Q.runSimpleQueryArrStart q ()
        parser = IRQ.prepareRowParser qr b
