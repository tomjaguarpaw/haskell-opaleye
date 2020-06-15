{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

-- | Inserts, updates and deletes
--
-- Please note that Opaleye currently only supports INSERT or UPDATE with
-- constant values, not the result of SELECTs.  That is, you can
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
                             -- | Do not use the export of
                             -- 'U.Unpackspec'.  It will not be
                             -- exported in version 0.7.
                             U.Unpackspec,
                             -- | Currently 'HSql.DoNothing' is the
                             -- only conflict action supported by
                             -- Opaleye.
                             HSql.OnConflict(..)) where

import qualified Opaleye.Field        as F
import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Internal.Print as Print
import qualified Opaleye.RunQuery as RQ
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Table as T
import qualified Opaleye.Internal.Table as TI
import           Opaleye.Internal.Column (Column(Column))
import           Opaleye.Internal.Helpers ((.:), (.:.), (.::.))
import           Opaleye.Internal.Manipulation (Updater(Updater))
import qualified Opaleye.Internal.Manipulation as MI
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Unpackspec as U
import           Opaleye.SqlTypes (SqlBool)

import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Print as HPrint
import qualified Opaleye.Internal.HaskellDB.Sql.Default as SD
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as SG

import qualified Database.PostgreSQL.Simple as PGS

import qualified Data.Profunctor.Product.Default as D

import           Data.Int (Int64)
import           Data.String (fromString)
import qualified Data.List.NonEmpty as NEL

-- * Run a manipulation

-- | Run the 'Insert'.  To create an 'Insert' use the 'Insert'
-- constructor.
runInsert_ :: PGS.Connection
           -- ^
           -> Insert haskells
           -- ^
           -> IO haskells
           -- ^ Returns a type that depends on the 'MI.Returning' that
           -- you provided when creating the 'Insert'.
runInsert_ conn i = case i of
  Insert table_ rows_ returning_ onConflict_ ->
    let insert = case (returning_, onConflict_) of
          (MI.Count, Nothing) ->
            runInsertMany
          (MI.Count, Just HSql.DoNothing) ->
            runInsertManyOnConflictDoNothing
          (MI.ReturningExplicit qr f, oc) ->
            \c t r -> MI.runInsertManyReturningExplicit qr c t r f oc
    in insert conn table_ rows_

-- | Run the 'Update'.  To create an 'Update' use the 'Update'
-- constructor.
runUpdate_ :: PGS.Connection
           -- ^
           -> Update haskells
           -- ^
           -> IO haskells
           -- ^ Returns a type that depends on the 'MI.Returning' that
           -- you provided when creating the 'Update'.
runUpdate_ conn i = case i of
  Update table_ updateWith_ where_ returning_ ->
    let update = case returning_ of
          MI.Count ->
            runUpdate
          MI.ReturningExplicit qr f ->
            \c t u w -> runUpdateReturningExplicit qr c t u w f
    in update conn table_ updateWith_ where_

-- | Run the 'Delete'.  To create an 'Delete' use the 'Delete'
-- constructor.
runDelete_ :: PGS.Connection
           -- ^
           -> Delete haskells
           -> IO haskells
           -- ^ Returns a type that depends on the 'MI.Returning' that
           -- you provided when creating the 'Delete'.
runDelete_ conn i = case i of
  Delete table_ where_ returning_ ->
    let delete = case returning_ of
          MI.Count ->
            runDelete
          MI.ReturningExplicit qr f ->
            \c t w -> MI.runDeleteReturningExplicit qr c t w f
    in delete conn table_ where_

-- * Create a manipulation

data Insert haskells = forall fieldsW fieldsR. Insert
   { iTable      :: T.Table fieldsW fieldsR
   , iRows       :: [fieldsW]
   , iReturning  :: MI.Returning fieldsR haskells
   , iOnConflict :: Maybe HSql.OnConflict
   -- ^ NB There is a clash of terminology between Haskell and
   -- Postgres.
   --
   --     * 'iOnConflict' @=@ 'Nothing' means omit @ON CONFLICT@ statement
   --
   --     * 'iOnConflict' @=@ 'Just' 'HSql.DoNothing' means @ON CONFLICT DO
   --        NOTHING@
   }

data Update haskells = forall fieldsW fieldsR. Update
   { uTable      :: T.Table fieldsW fieldsR
   , uUpdateWith :: fieldsR -> fieldsW
   -- ^ Be careful: providing 'Nothing' to a field created by
   -- 'Opaleye.Table.optional' updates the field to its default
   -- value.  Many users have been confused by this because they
   -- assume it means that the field is to be left unchanged.  For an
   -- easier time wrap your update function in 'updateEasy'.
   , uWhere      :: fieldsR -> F.Field SqlBool
   , uReturning  :: MI.Returning fieldsR haskells
   }

-- | A convenient wrapper for writing your update function
--
-- @uUpdateWith = updateEasy (\\... -> ...)@
updateEasy :: D.Default Updater fieldsR fieldsW
           => (fieldsR -> fieldsR)
           -- ^
           -> (fieldsR -> fieldsW)
updateEasy u = u' . u
  where Updater u' = D.def

data Delete haskells = forall fieldsW fieldsR. Delete
  { dTable     :: T.Table fieldsW fieldsR
  , dWhere     :: fieldsR -> F.Field SqlBool
  , dReturning :: MI.Returning fieldsR haskells
  }

-- ** Returning

-- | Return the number of rows inserted or updated
rCount :: MI.Returning fieldsR Int64
rCount = MI.Count

-- | Return a function of the inserted or updated rows
--
-- 'rReturning''s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- 'rReturning'.
rReturning :: D.Default RQ.QueryRunner fields haskells
           => (fieldsR -> fields)
           -- ^
           -> MI.Returning fieldsR [haskells]
rReturning = rReturningExplicit D.def

-- | Return a function of the inserted or updated rows.  Explicit
-- version.  You probably just want to use 'rReturning' instead.
rReturningExplicit :: RQ.QueryRunner fields haskells
                   -- ^
                   -> (fieldsR -> fields)
                   -- ^
                   -> MI.Returning fieldsR [haskells]
rReturningExplicit = MI.ReturningExplicit

-- * Deprecated versions

-- | Returns the number of rows inserted

{-# DEPRECATED runInsert
    "'runInsert' will be removed in version 0.7. \
    \Use 'runInsertMany' instead." #-}
runInsert :: PGS.Connection -> T.Table fields fields' -> fields -> IO Int64
runInsert conn = PGS.execute_ conn . fromString .: arrangeInsertSql

-- | @runInsertReturning@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runInsertReturning@.

{-# DEPRECATED runInsertReturning
    "'runInsertReturning' will be removed in version 0.7. \
    \Use 'runInsertManyReturning' instead." #-}
runInsertReturning :: (D.Default RQ.QueryRunner fieldsReturned haskells)
                   => PGS.Connection
                   -> T.Table fieldsW fieldsR
                   -> fieldsW
                   -> (fieldsR -> fieldsReturned)
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
arrangeInsertMany t columns = MI.arrangeInsertMany t columns Nothing

{-# DEPRECATED arrangeInsertManySql
    "You probably want 'runInsertMany' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertManySql :: T.Table columns a -> NEL.NonEmpty columns -> String
arrangeInsertManySql t c  = MI.arrangeInsertManySql t c Nothing

{-# DEPRECATED arrangeUpdate
    "You probably want 'runUpdate' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdate :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column SqlBool)
              -> HSql.SqlUpdate
arrangeUpdate t update cond =
  SG.sqlUpdate SD.defaultSqlGenerator
               (PQ.tiToSqlTable (TI.tableIdentifier t))
               [condExpr] (update' tableCols)
  where TI.TableProperties writer (TI.View tableCols) = TI.tableColumns t
        update' = map (\(x, y) -> (y, x)) . TI.runWriter writer . update
        Column condExpr = cond tableCols

{-# DEPRECATED arrangeUpdateSql
    "You probably want 'runUpdate' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdateSql :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column SqlBool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:. arrangeUpdate

{-# DEPRECATED arrangeDelete
    "You probably want 'runDelete' instead. \
    \Will be removed in version 0.7." #-}
arrangeDelete :: T.Table a columnsR -> (columnsR -> Column SqlBool) -> HSql.SqlDelete
arrangeDelete = MI.arrangeDelete

{-# DEPRECATED arrangeDeleteSql
    "You probably want 'runDelete' instead. \
    \Will be removed in version 0.7." #-}
arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Column SqlBool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

{-# DEPRECATED arrangeInsertManyReturning
    "You probably want 'runInsertMany' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertManyReturning :: U.Unpackspec columnsReturned ignored
                           -> T.Table columnsW columnsR
                           -> NEL.NonEmpty columnsW
                           -> (columnsR -> columnsReturned)
                           -> Sql.Returning HSql.SqlInsert
arrangeInsertManyReturning unpackspec t columns returningf =
  MI.arrangeInsertManyReturning unpackspec t columns returningf Nothing

{-# DEPRECATED arrangeInsertManyReturningSql
    "You probably want 'runInsertManyReturning' instead. \
    \Will be removed in version 0.7." #-}
arrangeInsertManyReturningSql :: U.Unpackspec columnsReturned ignored
                              -> T.Table columnsW columnsR
                              -> NEL.NonEmpty columnsW
                              -> (columnsR -> columnsReturned)
                              -> String
arrangeInsertManyReturningSql u t c r =
  MI.arrangeInsertManyReturningSql u t c r Nothing

{-# DEPRECATED arrangeUpdateReturning
    "You probably want 'runUpdateReturning' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdateReturning :: U.Unpackspec columnsReturned ignored
                       -> T.Table columnsW columnsR
                       -> (columnsR -> columnsW)
                       -> (columnsR -> Column SqlBool)
                       -> (columnsR -> columnsReturned)
                       -> Sql.Returning HSql.SqlUpdate
arrangeUpdateReturning unpackspec t updatef cond returningf =
  Sql.Returning update returningSEs
  where update = arrangeUpdate t updatef cond
        TI.View columnsR = TI.tableColumnsView (TI.tableColumns t)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

{-# DEPRECATED arrangeUpdateReturningSql
    "You probably want 'runUpdateReturning' instead. \
    \Will be removed in version 0.7." #-}
arrangeUpdateReturningSql :: U.Unpackspec columnsReturned ignored
                          -> T.Table columnsW columnsR
                          -> (columnsR -> columnsW)
                          -> (columnsR -> Column SqlBool)
                          -> (columnsR -> columnsReturned)
                          -> String
arrangeUpdateReturningSql =
  show . Print.ppUpdateReturning .::. arrangeUpdateReturning

-- | Insert rows into a table with @ON CONFLICT DO NOTHING@
{-# DEPRECATED runInsertManyOnConflictDoNothing "Use 'runInsert_'.  Will be removed in version 0.8." #-}
runInsertManyOnConflictDoNothing :: PGS.Connection
                                 -- ^
                                 -> T.Table columns columns'
                                 -- ^ Table to insert into
                                 -> [columns]
                                 -- ^ Rows to insert
                                 -> IO Int64
                                 -- ^ Number of rows inserted
runInsertManyOnConflictDoNothing conn table_ columns =
  case NEL.nonEmpty columns of
    -- Inserting the empty list is just the same as returning 0
    Nothing       -> return 0
    Just columns' -> (PGS.execute_ conn . fromString .:. MI.arrangeInsertManySql)
                         table_ columns' (Just HSql.DoNothing)

-- | Insert rows into a table with @ON CONFLICT DO NOTHING@ and
-- return a function of the inserted rows
--
-- @runInsertManyReturningOnConflictDoNothing@'s use of the
-- 'D.Default' typeclass means that the compiler will have trouble
-- inferring types.  It is strongly recommended that you provide full
-- type signatures when using it.
{-# DEPRECATED runInsertManyReturningOnConflictDoNothing "Use 'runInsert_'. Will be removed in version 0.8." #-}
runInsertManyReturningOnConflictDoNothing
  :: (D.Default RQ.QueryRunner columnsReturned haskells)
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
runInsertManyReturningOnConflictDoNothing =
  runInsertManyReturningOnConflictDoNothingExplicit D.def

-- | Use 'runInsert_' instead.   Will be deprecated in version 0.7.
runInsertMany :: PGS.Connection
              -- ^
              -> T.Table columns columns'
              -- ^ Table to insert into
              -> [columns]
              -- ^ Rows to insert
              -> IO Int64
              -- ^ Number of rows inserted
runInsertMany conn t columns = case NEL.nonEmpty columns of
  -- Inserting the empty list is just the same as returning 0
  Nothing       -> return 0
  Just columns' -> (PGS.execute_ conn . fromString .: arrangeInsertManySql) t columns'

-- | Use 'runInsert_' instead.   Will be deprecated in version 0.7.
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

-- | Use 'runInsert_' instead.   Will be deprecated in version 0.7.
runInsertReturningExplicit :: RQ.QueryRunner columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table columnsW columnsR
                           -> columnsW
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runInsertReturningExplicit qr conn t =
  runInsertManyReturningExplicit qr conn t . return

-- | Use 'runInsert_' instead.   Will be deprecated in version 0.7.
runInsertManyReturningExplicit :: RQ.QueryRunner columnsReturned haskells
                               -> PGS.Connection
                               -> T.Table columnsW columnsR
                               -> [columnsW]
                               -> (columnsR -> columnsReturned)
                               -> IO [haskells]
runInsertManyReturningExplicit qr conn t columns f =
  MI.runInsertManyReturningExplicit qr conn t columns f Nothing

-- | Use 'runInsert_' instead.   Will be deprecated in version 0.7.
runInsertManyReturningOnConflictDoNothingExplicit
  :: RQ.QueryRunner columnsReturned haskells
  -> PGS.Connection
  -> T.Table columnsW columnsR
  -> [columnsW]
  -> (columnsR -> columnsReturned)
  -> IO [haskells]
runInsertManyReturningOnConflictDoNothingExplicit qr conn t columns f =
  MI.runInsertManyReturningExplicit qr conn t columns f (Just HSql.DoNothing)

-- | Use 'runUpdate_' instead.   Will be deprecated in version 0.7.
runUpdateEasy :: D.Default Updater columnsR columnsW
              => PGS.Connection
              -> T.Table columnsW columnsR
              -- ^ Table to update
              -> (columnsR -> columnsR)
              -- ^ Update function to apply to chosen rows
              -> (columnsR -> Column SqlBool)
              -- ^ Predicate function @f@ to choose which rows to update.
              -- 'runUpdate' will update rows for which @f@ returns @TRUE@
              -- and leave unchanged rows for which @f@ returns @FALSE@.
              -> IO Int64
              -- ^ The number of rows updated
runUpdateEasy conn table_ u = runUpdate conn table_ (u' . u)
  where Updater u' = D.def

-- | Use 'runUpdate_' instead.   Will be deprecated in version 0.7.
runUpdate :: PGS.Connection
          -> T.Table columnsW columnsR
          -- ^ Table to update
          -> (columnsR -> columnsW)
          -- ^ Update function to apply to chosen rows
          -> (columnsR -> Column SqlBool)
          -- ^ Predicate function @f@ to choose which rows to update.
          -- 'runUpdate' will update rows for which @f@ returns @TRUE@
          -- and leave unchanged rows for which @f@ returns @FALSE@.
          -> IO Int64
          -- ^ The number of rows updated
runUpdate conn = PGS.execute_ conn . fromString .:. arrangeUpdateSql

-- | Use 'runUpdate_' instead.   Will be deprecated in version 0.7.
runUpdateReturning :: (D.Default RQ.QueryRunner columnsReturned haskells)
                   => PGS.Connection
                   -- ^
                   -> T.Table columnsW columnsR
                   -- ^ Table to update
                   -> (columnsR -> columnsW)
                   -- ^ Update function to apply to chosen rows
                   -> (columnsR -> Column SqlBool)
                   -- ^ Predicate function @f@ to choose which rows to
                   -- update.  'runUpdate' will update rows for which
                   -- @f@ returns @TRUE@ and leave unchanged rows for
                   -- which @f@ returns @FALSE@.
                   -> (columnsR -> columnsReturned)
                   -- ^ Functon @g@ to apply to the updated rows
                   -> IO [haskells]
                   -- ^ Returned rows after @g@ has been applied
runUpdateReturning = runUpdateReturningExplicit D.def

-- | Use 'runUpdate_' instead.   Will be deprecated in version 0.7.
runUpdateReturningExplicit :: RQ.QueryRunner columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table columnsW columnsR
                           -> (columnsR -> columnsW)
                           -> (columnsR -> Column SqlBool)
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runUpdateReturningExplicit qr conn t update cond r =
  PGS.queryWith_ parser conn
                 (fromString (arrangeUpdateReturningSql u t update cond r))
  where IRQ.QueryRunner u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tableColumnsView (TI.tableColumns t)

-- | Use 'runDelete_' instead.  Will be deprecated in 0.7.
runDelete :: PGS.Connection
          -- ^
          -> T.Table a columnsR
          -- ^ Table to delete rows from
          -> (columnsR -> Column SqlBool)
          -- ^ Predicate function @f@ to choose which rows to delete.
          -- 'runDelete' will delete rows for which @f@ returns @TRUE@
          -- and leave unchanged rows for which @f@ returns @FALSE@.
          -> IO Int64
          -- ^ The number of rows deleted
runDelete conn = PGS.execute_ conn . fromString .: arrangeDeleteSql
