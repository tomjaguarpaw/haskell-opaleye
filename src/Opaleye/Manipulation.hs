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

module Opaleye.Manipulation (-- * Insert
                             runInsert_,
                             Insert(..),
                             -- * Update
                             runUpdate_,
                             Update(..),
                             updateEasy,
                             -- * Delete
                             runDelete_,
                             Delete(..),
                             -- * Returning
                             MI.Returning,
                             rCount,
                             rReturning,
                             rReturningI,
                             rReturningExplicit,
                             -- * On conflict
                             -- | Currently 'HSql.DoNothing' is the
                             -- only conflict action supported by
                             -- Opaleye.
                             HSql.OnConflict(..)) where

import qualified Opaleye.Field        as F
import qualified Opaleye.RunSelect as RS
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Table as T
import qualified Opaleye.Internal.Table as TI
import           Opaleye.Internal.Column (Column)
import           Opaleye.Internal.Helpers ((.:), (.:.))
import           Opaleye.Internal.Inferrable (Inferrable, runInferrable)
import           Opaleye.Internal.Manipulation (Updater(Updater))
import qualified Opaleye.Internal.Manipulation as MI
import           Opaleye.SqlTypes (SqlBool)

import qualified Opaleye.Internal.HaskellDB.Sql as HSql

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
            \c -> PGS.execute_ c . fromString .:. MI.arrangeUpdateSql
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
  Delete table_ where_ returning_ -> case returning_ of
          MI.Count ->
            PGS.execute_ conn (fromString (MI.arrangeDeleteSql table_ where_))
          MI.ReturningExplicit qr f ->
            MI.runDeleteReturningExplicit qr conn table_ where_ f

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
-- 'rReturning''s use of the @'D.Default' 'Opaleye.RunSelect.FromFields'@
-- typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- 'rReturning'.
rReturning :: D.Default RS.FromFields fields haskells
           => (fieldsR -> fields)
           -- ^
           -> MI.Returning fieldsR [haskells]
rReturning = rReturningExplicit D.def

-- | Like 'rReturning' but with better inference properties.  On the
-- other hand the mapping from SQL fields to Haskell types is less
-- flexible.
rReturningI :: D.Default (Inferrable RS.FromFields) fields haskells
            => (fieldsR -> fields)
            -- ^
            -> MI.Returning fieldsR [haskells]
rReturningI = rReturningExplicit (runInferrable D.def)

-- | Return a function of the inserted or updated rows.  Explicit
-- version.  You probably just want to use 'rReturning' instead.
rReturningExplicit :: RS.FromFields fields haskells
                   -- ^
                   -> (fieldsR -> fields)
                   -- ^
                   -> MI.Returning fieldsR [haskells]
rReturningExplicit = MI.ReturningExplicit

-- * Deprecated versions

-- | Insert rows into a table with @ON CONFLICT DO NOTHING@
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
  Just columns' -> (PGS.execute_ conn . fromString .: MI.arrangeInsertManySqlI) t columns'

runUpdateReturningExplicit :: RS.FromFields columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table columnsW columnsR
                           -> (columnsR -> columnsW)
                           -> (columnsR -> Column SqlBool)
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runUpdateReturningExplicit qr conn t update cond r =
  PGS.queryWith_ parser conn
                 (fromString (MI.arrangeUpdateReturningSql u t update cond r))
  where IRQ.QueryRunner u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tableColumnsView (TI.tableColumns t)
