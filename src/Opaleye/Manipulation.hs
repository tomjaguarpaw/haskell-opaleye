{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Inserts, updates and deletes
--
-- Please note that you currently you can only INSERT or UPDATE with
-- constant values, not the result of SELECTS.  That is, you can
-- generate SQL of the form
--
-- @
-- INSERT INTO thetable ('John', 1);
-- @
--
-- but not
--
-- @
-- INSERT INTO thetable
--    SELECT 'John',
--    (SELECT num FROM thetable ORDER BY num DESC LIMIT 1) + 1;
-- @

module Opaleye.Manipulation (module Opaleye.Manipulation,
                             -- * Other
                             U.Unpackspec) where

import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Internal.Print as Print
import qualified Opaleye.RunQuery as RQ
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Table as T
import qualified Opaleye.Internal.Table as TI
import           Opaleye.Internal.Column (Column(Column))
import           Opaleye.Internal.Helpers ((.:), (.:.), (.::), (.::.))
import           Opaleye.Internal.Manipulation (Updater(Updater))
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Unpackspec as U
import           Opaleye.PGTypes (PGBool)

import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Print as HPrint
import qualified Opaleye.Internal.HaskellDB.Sql.Default as SD
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as SG

import qualified Database.PostgreSQL.Simple as PGS

import qualified Data.Profunctor.Product.Default as D

import           Data.Int (Int64)
import           Data.String (fromString)
import qualified Data.List.NonEmpty as NEL

-- * Manipulation functions

-- | Insert rows into a table
runInsertMany :: PGS.Connection
              -- ^
              -> T.Table columns columns'
              -- ^ Table to insert into
              -> [columns]
              -- ^ Rows to insert
              -> IO Int64
              -- ^ Number of rows inserted
runInsertMany conn table columns = case NEL.nonEmpty columns of
  -- Inserting the empty list is just the same as returning 0
  Nothing       -> return 0
  Just columns' -> (PGS.execute_ conn . fromString .: arrangeInsertManySql) table columns'

-- | Insert rows into a table and return a function of the inserted rows
--
-- @runInsertManyReturning@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runInsertManyReturning@.
runInsertManyReturning :: (D.Default RQ.QueryRunner columnsReturned haskells)
                       => PGS.Connection
                       -- ^
                       -> T.Table columnsW columnsR
                       -- ^ Table to insert into
                       -> [columnsW]
                       -- ^ Rows to insert
                       -> (columnsR -> columnsReturned)
                       -- ^ Function @f@ to apply to the inserted rows
                       -> IO [haskells]
                       -- ^ Returned rows after @f@ has been applied
runInsertManyReturning = runInsertManyReturningExplicit D.def

-- | Update rows in a table.
--
-- (N.B. 'runUpdateEasy''s \"returning\" counterpart
-- \"@runUpdateEasyReturning@\" hasn't been implemented.  File an
-- issue if you want it!)
runUpdateEasy :: D.Default Updater columnsR columnsW
              => PGS.Connection
              -> T.Table columnsW columnsR
              -- ^ Table to update
              -> (columnsR -> columnsR)
              -- ^ Update function to apply to chosen rows
              -> (columnsR -> Column PGBool)
              -- ^ Predicate function @f@ to choose which rows to update.
              -- 'runUpdate' will update rows for which @f@ returns @TRUE@
              -- and leave unchanged rows for which @f@ returns @FALSE@.
              -> IO Int64
              -- ^ The number of rows updated
runUpdateEasy conn table u = runUpdate conn table (u' . u)
  where Updater u' = D.def

-- | Update rows in a table.  You'll probably find it more convenient
-- to use 'runUpdateEasy' (although 'runUpdate' provides more
-- fine-grained control if you need it).
--
-- Be careful: providing 'Nothing' to a column created by @optional@
-- updates the column to its default value.  Many users have been
-- confused by this because they assume it means that the column is to
-- be left unchanged.
runUpdate :: PGS.Connection
          -> T.Table columnsW columnsR
          -- ^ Table to update
          -> (columnsR -> columnsW)
          -- ^ Update function to apply to chosen rows
          -> (columnsR -> Column PGBool)
          -- ^ Predicate function @f@ to choose which rows to update.
          -- 'runUpdate' will update rows for which @f@ returns @TRUE@
          -- and leave unchanged rows for which @f@ returns @FALSE@.
          -> IO Int64
          -- ^ The number of rows updated
runUpdate conn = PGS.execute_ conn . fromString .:. arrangeUpdateSql


-- | Update rows in a table and return a function of the updated rows
--
-- Be careful: providing 'Nothing' to a column created by @optional@
-- updates the column to its default value.  Many users have been
-- confused by this because they assume it means that the column is to
-- be left unchanged.
--
-- @runUpdateReturning@'s use of the 'D.Default' typeclass means
-- that the compiler will have trouble inferring types.  It is
-- strongly recommended that you provide full type signatures when
-- using @runInsertReturning@.
runUpdateReturning :: (D.Default RQ.QueryRunner columnsReturned haskells)
                   => PGS.Connection
                   -- ^
                   -> T.Table columnsW columnsR
                   -- ^ Table to update
                   -> (columnsR -> columnsW)
                   -- ^ Update function to apply to chosen rows
                   -> (columnsR -> Column PGBool)
                   -- ^ Predicate function @f@ to choose which rows to
                   -- update.  'runUpdate' will update rows for which
                   -- @f@ returns @TRUE@ and leave unchanged rows for
                   -- which @f@ returns @FALSE@.
                   -> (columnsR -> columnsReturned)
                   -- ^ Functon @g@ to apply to the updated rows
                   -> IO [haskells]
                   -- ^ Returned rows after @g@ has been applied
runUpdateReturning = runUpdateReturningExplicit D.def

-- | Delete rows from a table
runDelete :: PGS.Connection
          -- ^
          -> T.Table a columnsR
          -- ^ Table to delete rows from
          -> (columnsR -> Column PGBool)
          -- ^ Predicate function @f@ to choose which rows to delete.
          -- 'runDelete' will delete rows for which @f@ returns @TRUE@
          -- and leave unchanged rows for which @f@ returns @FALSE@.
          -> IO Int64
          -- ^ The number of rows deleted
runDelete conn = PGS.execute_ conn . fromString .: arrangeDeleteSql

-- * Explicit versions

-- | You probably don't need this, but can just use
-- 'runInsertReturning' instead.  You only need it if you want to run
-- an INSERT RETURNING statement but need to be explicit about the
-- 'QueryRunner'.
runInsertReturningExplicit :: RQ.QueryRunner columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table columnsW columnsR
                           -> columnsW
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runInsertReturningExplicit qr conn t =
  runInsertManyReturningExplicit qr conn t . return

-- | You probably don't need this, but can just use
-- 'runInsertManyReturning' instead.  You only need it if you want to
-- run an UPDATE RETURNING statement but need to be explicit about the
-- 'QueryRunner'.
runInsertManyReturningExplicit :: RQ.QueryRunner columnsReturned haskells
                               -> PGS.Connection
                               -> T.Table columnsW columnsR
                               -> [columnsW]
                               -> (columnsR -> columnsReturned)
                               -> IO [haskells]
runInsertManyReturningExplicit qr conn t columns r =
  case NEL.nonEmpty columns of
    Nothing       -> return []
    Just columns' -> PGS.queryWith_ parser conn
                       (fromString
                        (arrangeInsertManyReturningSql u t columns' r))
  where IRQ.QueryRunner u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tableColumnsView (TI.tableColumns t)
        -- This method of getting hold of the return type feels a bit
        -- suspect.  I haven't checked it for validity.

-- | You probably don't need this, but can just use
-- 'runUpdateReturning' instead.  You only need it if you want to run
-- an UPDATE RETURNING statement but need to be explicit about the
-- 'QueryRunner'.
runUpdateReturningExplicit :: RQ.QueryRunner columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table columnsW columnsR
                           -> (columnsR -> columnsW)
                           -> (columnsR -> Column PGBool)
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runUpdateReturningExplicit qr conn t update cond r =
  PGS.queryWith_ parser conn
                 (fromString (arrangeUpdateReturningSql u t update cond r))
  where IRQ.QueryRunner u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tableColumnsView (TI.tableColumns t)

-- * Deprecated versions

-- | Returns the number of rows inserted

{-# DEPRECATED runInsert
    "'runInsert' will be removed in version 0.7. \
    \Use 'runInsertMany' instead." #-}
runInsert :: PGS.Connection -> T.Table columns columns' -> columns -> IO Int64
runInsert conn = PGS.execute_ conn . fromString .: arrangeInsertSql

-- | @runInsertReturning@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runInsertReturning@.

{-# DEPRECATED runInsertReturning
    "'runInsertReturning' will be removed in version 0.7. \
    \Use 'runInsertManyReturning' instead." #-}
runInsertReturning :: (D.Default RQ.QueryRunner columnsReturned haskells)
                   => PGS.Connection
                   -> T.Table columnsW columnsR
                   -> columnsW
                   -> (columnsR -> columnsReturned)
                   -> IO [haskells]
runInsertReturning = runInsertReturningExplicit D.def

{-# DEPRECATED arrangeInsert
    "You probably want 'runInsertMany' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsert :: T.Table columns a -> columns -> HSql.SqlInsert
arrangeInsert t c = arrangeInsertMany t (return c)

{-# DEPRECATED arrangeInsertSql
    "You probably want 'runInsertMany' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertSql :: T.Table columns a -> columns -> String
arrangeInsertSql = show . HPrint.ppInsert .: arrangeInsert

{-# DEPRECATED arrangeInsertMany
    "You probably want 'runInsertMany' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertMany :: T.Table columns a -> NEL.NonEmpty columns -> HSql.SqlInsert
arrangeInsertMany table columns = insert
  where writer = TI.tableColumnsWriter (TI.tableColumns table)
        (columnExprs, columnNames) = TI.runWriter' writer columns
        insert = SG.sqlInsert SD.defaultSqlGenerator
                      (PQ.tiToSqlTable (TI.tableIdentifier table))
                      columnNames columnExprs

{-# DEPRECATED arrangeInsertManySql
    "You probably want 'runInsertMany' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertManySql :: T.Table columns a -> NEL.NonEmpty columns -> String
arrangeInsertManySql = show . HPrint.ppInsert .: arrangeInsertMany

{-# DEPRECATED arrangeUpdate
    "You probably want 'runUpdate' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdate :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> HSql.SqlUpdate
arrangeUpdate table update cond =
  SG.sqlUpdate SD.defaultSqlGenerator
               (PQ.tiToSqlTable (TI.tableIdentifier table))
               [condExpr] (update' tableCols)
  where TI.TableProperties writer (TI.View tableCols) = TI.tableColumns table
        update' = map (\(x, y) -> (y, x)) . TI.runWriter writer . update
        Column condExpr = cond tableCols

{-# DEPRECATED arrangeUpdateSql
    "You probably want 'runUpdate' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdateSql :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:. arrangeUpdate

{-# DEPRECATED arrangeDelete
    "You probably want 'runDelete' instead. \
    \Will be removed in version 0.7." #-}
arrangeDelete :: T.Table a columnsR -> (columnsR -> Column PGBool) -> HSql.SqlDelete
arrangeDelete table cond =
  SG.sqlDelete SD.defaultSqlGenerator (PQ.tiToSqlTable (TI.tableIdentifier table)) [condExpr]
  where Column condExpr = cond tableCols
        TI.View tableCols = TI.tableColumnsView (TI.tableColumns table)

{-# DEPRECATED arrangeDeleteSql
    "You probably want 'runDelete' instead. \
    \Will be removed in version 0.7." #-}
arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Column PGBool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

{-# DEPRECATED arrangeInsertManyReturning
    "You probably want 'runInsertMany' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertManyReturning :: U.Unpackspec columnsReturned ignored
                           -> T.Table columnsW columnsR
                           -> NEL.NonEmpty columnsW
                           -> (columnsR -> columnsReturned)
                           -> Sql.Returning HSql.SqlInsert
arrangeInsertManyReturning unpackspec table columns returningf =
  Sql.Returning insert returningSEs
  where insert = arrangeInsertMany table columns
        TI.View columnsR = TI.tableColumnsView (TI.tableColumns table)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

{-# DEPRECATED arrangeInsertManyReturningSql
    "You probably want 'runInsertManyReturning' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertManyReturningSql :: U.Unpackspec columnsReturned ignored
                              -> T.Table columnsW columnsR
                              -> NEL.NonEmpty columnsW
                              -> (columnsR -> columnsReturned)
                              -> String
arrangeInsertManyReturningSql =
  show . Print.ppInsertReturning .:: arrangeInsertManyReturning

{-# DEPRECATED arrangeUpdateReturning
    "You probably want 'runUpdateReturning' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdateReturning :: U.Unpackspec columnsReturned ignored
                       -> T.Table columnsW columnsR
                       -> (columnsR -> columnsW)
                       -> (columnsR -> Column PGBool)
                       -> (columnsR -> columnsReturned)
                       -> Sql.Returning HSql.SqlUpdate
arrangeUpdateReturning unpackspec table updatef cond returningf =
  Sql.Returning update returningSEs
  where update = arrangeUpdate table updatef cond
        TI.View columnsR = TI.tableColumnsView (TI.tableColumns table)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

{-# DEPRECATED arrangeUpdateReturningSql
    "You probably want 'runUpdateReturning' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdateReturningSql :: U.Unpackspec columnsReturned ignored
                          -> T.Table columnsW columnsR
                          -> (columnsR -> columnsW)
                          -> (columnsR -> Column PGBool)
                          -> (columnsR -> columnsReturned)
                          -> String
arrangeUpdateReturningSql =
  show . Print.ppUpdateReturning .::. arrangeUpdateReturning
