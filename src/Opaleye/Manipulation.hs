{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.TableMaker as TM
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
runInsertManyReturning :: (D.Default RQ.QueryRunner columnsReturned haskells,
                          D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                       => PGS.Connection
                       -- ^
                       -> T.Table columnsW tableColumns
                       -- ^ Table to insert into
                       -> [columnsW]
                       -- ^ Rows to insert
                       -> (columnsR -> columnsReturned)
                       -- ^ Function @f@ to apply to the inserted rows
                       -> IO [haskells]
                       -- ^ Returned rows after @f@ has been applied
runInsertManyReturning = runInsertManyReturningExplicit D.def

-- | Update rows in a table
runUpdate :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
          => PGS.Connection
          -> T.Table columnsW tableColumns
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
-- @runUpdateReturning@'s use of the 'D.Default' typeclass means
-- that the compiler will have trouble inferring types.  It is
-- strongly recommended that you provide full type signatures when
-- using @runInsertReturning@.
runUpdateReturning :: (D.Default RQ.QueryRunner columnsReturned haskells,
                       D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                   => PGS.Connection
                   -- ^
                   -> T.Table columnsW tableColumns
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
runDelete :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
          => PGS.Connection
          -- ^
          -> T.Table a tableColumns
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
runInsertReturningExplicit :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                           => RQ.QueryRunner columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table columnsW tableColumns
                           -> columnsW
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runInsertReturningExplicit qr conn t =
  runInsertManyReturningExplicit qr conn t . return

-- | You probably don't need this, but can just use
-- 'runInsertManyReturning' instead.  You only need it if you want to
-- run an UPDATE RETURNING statement but need to be explicit about the
-- 'QueryRunner'.
runInsertManyReturningExplicit :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                               => RQ.QueryRunner columnsReturned haskells
                               -> PGS.Connection
                               -> T.Table columnsW tableColumns
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
        parser = IRQ.prepareRowParser qr ((r . TI.runTableProjector D.def) v)
        TI.Table _ (TI.TableProperties _ (TI.View v)) = t
        -- This method of getting hold of the return type feels a bit
        -- suspect.  I haven't checked it for validity.

-- | You probably don't need this, but can just use
-- 'runUpdateReturning' instead.  You only need it if you want to run
-- an UPDATE RETURNING statement but need to be explicit about the
-- 'QueryRunner'.
runUpdateReturningExplicit :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                           => RQ.QueryRunner columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table columnsW tableColumns
                           -> (columnsR -> columnsW)
                           -> (columnsR -> Column PGBool)
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runUpdateReturningExplicit qr conn t update cond r =
  PGS.queryWith_ parser conn
                 (fromString (arrangeUpdateReturningSql u t update cond r))
  where IRQ.QueryRunner u _ _ = qr
        parser = IRQ.prepareRowParser qr (r . TI.runTableProjector D.def $ v)
        TI.Table _ (TI.TableProperties _ (TI.View v)) = t

-- * Deprecated versions

-- | Returns the number of rows inserted
--
-- This will be deprecated in version 0.6.  Use 'runInsertMany'
-- instead.
runInsert :: PGS.Connection -> T.Table columns columns' -> columns -> IO Int64
runInsert conn = PGS.execute_ conn . fromString .: arrangeInsertSql

-- | @runInsertReturning@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runInsertReturning@.
--
-- This will be deprecated in version 0.6.  Use
-- 'runInsertManyReturning' instead.
runInsertReturning :: (D.Default RQ.QueryRunner columnsReturned haskells, 
                       D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                   => PGS.Connection
                   -> T.Table columnsW tableColumns
                   -> columnsW
                   -> (columnsR -> columnsReturned)
                   -> IO [haskells]
runInsertReturning = runInsertReturningExplicit D.def

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeInsert :: T.Table columns a -> columns -> HSql.SqlInsert
arrangeInsert t c = arrangeInsertMany t (return c)

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeInsertSql :: T.Table columns a -> columns -> String
arrangeInsertSql = show . HPrint.ppInsert .: arrangeInsert

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeInsertMany :: T.Table columns a -> NEL.NonEmpty columns -> HSql.SqlInsert
arrangeInsertMany table columns = insert
  where writer = TI.tablePropertiesWriter (TI.tableProperties table)
        (columnExprs, columnNames) = TI.runWriter' writer columns
        insert = SG.sqlInsert SD.defaultSqlGenerator
                      (PQ.tiToSqlTable (TI.tableIdentifier table))
                      columnNames columnExprs

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeInsertManySql :: T.Table columns a -> NEL.NonEmpty columns -> String
arrangeInsertManySql = show . HPrint.ppInsert .: arrangeInsertMany

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeUpdate :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
              => T.Table columnsW tableColumns
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> HSql.SqlUpdate
arrangeUpdate table update cond =
  SG.sqlUpdate SD.defaultSqlGenerator
               (PQ.tiToSqlTable (TI.tableIdentifier table))
               [condExpr] (update' tableCols)
  where TI.TableProperties writer (TI.View tableCols) = TI.tableProperties table
        update' = map (\(x, y) -> (y, x)) . TI.runWriter writer . update . TI.runTableProjector tableProjector
        Column condExpr = cond . TI.runTableProjector tableProjector $ tableCols
        tableProjector = D.def

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.

arrangeUpdateSql ::
              (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
              => T.Table columnsW tableColumns
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:. arrangeUpdate

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeDelete :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
              => T.Table a tableColumns -> (columnsR -> Column PGBool) -> HSql.SqlDelete
arrangeDelete table cond =
  SG.sqlDelete SD.defaultSqlGenerator (PQ.tiToSqlTable (TI.tableIdentifier table)) [condExpr]
  where Column condExpr = (cond . TI.runTableProjector D.def) tableCols
        TI.View tableCols = TI.tablePropertiesView (TI.tableProperties table)

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeDeleteSql :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                 => T.Table a tableColumns -> (columnsR -> Column PGBool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeInsertManyReturning :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                           => U.Unpackspec columnsReturned ignored
                           -> T.Table columnsW tableColumns
                           -> NEL.NonEmpty columnsW
                           -> (columnsR -> columnsReturned)
                           -> Sql.Returning HSql.SqlInsert
arrangeInsertManyReturning unpackspec table columns returningf =
  Sql.Returning insert returningSEs
  where insert = arrangeInsertMany table columns
        TI.View columnsR = TI.tablePropertiesView (TI.tableProperties table)
        returningPEs = U.collectPEs unpackspec ((returningf . TI.runTableProjector D.def) columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeInsertManyReturningSql :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                              => U.Unpackspec columnsReturned ignored
                              -> T.Table columnsW tableColumns
                              -> NEL.NonEmpty columnsW
                              -> (columnsR -> columnsReturned)
                              -> String
arrangeInsertManyReturningSql =
  show . Print.ppInsertReturning .:: arrangeInsertManyReturning

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeUpdateReturning :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                       => U.Unpackspec columnsReturned ignored
                       -> T.Table columnsW tableColumns
                       -> (columnsR -> columnsW)
                       -> (columnsR -> Column PGBool)
                       -> (columnsR -> columnsReturned)
                       -> Sql.Returning HSql.SqlUpdate
arrangeUpdateReturning unpackspec table updatef cond returningf =
  Sql.Returning update returningSEs
  where update = arrangeUpdate table updatef cond
        TI.View columnsR = TI.tablePropertiesView (TI.tableProperties table)
        returningPEs = U.collectPEs unpackspec (returningf . TI.runTableProjector D.def $ columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
arrangeUpdateReturningSql :: (D.Default TM.TableProjector tableColumns columnsR, T.ColumnFromTableColumn tableColumns ~ columnsR)
                          => U.Unpackspec columnsReturned ignored
                          -> T.Table columnsW tableColumns
                          -> (columnsR -> columnsW)
                          -> (columnsR -> Column PGBool)
                          -> (columnsR -> columnsReturned)
                          -> String
arrangeUpdateReturningSql =
  show . Print.ppUpdateReturning .::. arrangeUpdateReturning
