{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

-- | Inserts, updates and deletes
--
-- Please note that Opaleye currently only supports INSERT or UPDATE with
-- constant values, not the result of SELECTs.  That is, you can
-- generate SQL of the form
--
-- @
-- INSERT INTO thetable (\'John\', 1);
-- @
--
-- but not
--
-- @
-- INSERT INTO thetable
--    SELECT \'John\',
--    (SELECT num FROM thetable ORDER BY num DESC LIMIT 1) + 1;
-- @

module Opaleye.Manipulation (-- * Insert
                             runInsert,
                             Insert(..),
                             -- * Update
                             runUpdate,
                             Update(..),
                             updateEasy,
                             -- * Delete
                             runDelete,
                             Delete(..),
                             -- * Returning
                             MI.Returning,
                             rCount,
                             rReturning,
                             rReturningI,
                             rReturningExplicit,
                             -- * On conflict
                             HSql.OnConflict,
                             doNothing,
                             doUpdate,
                             doUpdateEasy,
                             doUpdateAll,
                             -- * Deprecated
                             runInsert_,
                             runUpdate_,
                             runDelete_,
                             -- ** @DoNothing@
                             -- | Use 'doNothing' instead.
                             -- @DoNothing@ will be removed in
                             -- version 0.11.
                             HSql.OnConflict(HSql.DoNothing),
                             ) where

import qualified Opaleye.Field        as F
import qualified Opaleye.RunSelect as RS
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Table as T
import qualified Opaleye.Internal.Table as TI
import           Opaleye.Internal.Column (Column)
import           Opaleye.Internal.Helpers ((.:.))
import           Opaleye.Internal.Inferrable (Inferrable, runInferrable)
import           Opaleye.Internal.Manipulation (Updater(Updater))
import qualified Opaleye.Internal.Manipulation as MI
import qualified Opaleye.Internal.Unpackspec as U
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
runInsert  :: PGS.Connection
           -- ^
           -> Insert haskells
           -- ^
           -> IO haskells
           -- ^ Returns a type that depends on the 'MI.Returning' that
           -- you provided when creating the 'Insert'.
runInsert conn i = case i of
  Insert table_ rows_ returning_ onConflict_ ->
    let insert = case returning_ of
          MI.Count ->
            runInsertMany' onConflict_
          MI.ReturningExplicit qr f ->
            \c t r -> MI.runInsertManyReturningExplicit qr c t r f onConflict_
    in insert conn table_ rows_

{-# DEPRECATED runInsert_ "Use 'runInsert' instead.  Will be removed in 0.11." #-}
runInsert_ :: PGS.Connection
           -> Insert haskells
           -> IO haskells
runInsert_ = runInsert

-- | Run the 'Update'.  To create an 'Update' use the 'Update'
-- constructor.
runUpdate  :: PGS.Connection
           -- ^
           -> Update haskells
           -- ^
           -> IO haskells
           -- ^ Returns a type that depends on the 'MI.Returning' that
           -- you provided when creating the 'Update'.
runUpdate  conn i = case i of
  Update table_ updateWith_ where_ returning_ -> case returning_ of
          MI.Count ->
            PGS.execute_ conn (fromString (MI.arrangeUpdateSql table_ updateWith_ where_))
          MI.ReturningExplicit qr f ->
            runUpdateReturningExplicit qr conn table_ updateWith_ where_ f

{-# DEPRECATED runUpdate_ "Use 'runUpdate' instead.  Will be removed in 0.11." #-}
runUpdate_ :: PGS.Connection
           -> Update haskells
           -> IO haskells
runUpdate_ = runUpdate

-- | Run the 'Delete'.  To create an 'Delete' use the 'Delete'
-- constructor.
runDelete  :: PGS.Connection
           -- ^
           -> Delete haskells
           -> IO haskells
           -- ^ Returns a type that depends on the 'MI.Returning' that
           -- you provided when creating the 'Delete'.
runDelete conn i = case i of
  Delete table_ where_ returning_ -> case returning_ of
          MI.Count ->
            PGS.execute_ conn (fromString (MI.arrangeDeleteSql table_ where_))
          MI.ReturningExplicit qr f ->
            MI.runDeleteReturningExplicit qr conn table_ where_ f

{-# DEPRECATED runDelete_ "Use 'runDelete' instead.  Will be removed in 0.11." #-}
runDelete_ :: PGS.Connection
           -> Delete haskells
           -> IO haskells
runDelete_ = runDelete

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
   --     * 'iOnConflict' @=@ 'Just' 'HSql.doNothing' means @ON CONFLICT DO
   --        NOTHING@
   }

data Update haskells = forall fieldsW fieldsR. Update
   { uTable      :: T.Table fieldsW fieldsR
   , uUpdateWith :: fieldsR -> fieldsW
   -- ^ Be careful: providing 'Nothing' to a field created by
   -- 'Opaleye.Table.optionalTableField' updates the field to its default
   -- value.  Many users have been confused by this because they
   -- assume it means that the field is to be left unchanged.  For an
   -- easier time wrap your update function in 'updateEasy'.
   , uWhere      :: fieldsR -> F.Field SqlBool
   , uReturning  :: MI.Returning fieldsR haskells
   }

-- | A convenient wrapper for writing your update function.
-- @updateEasy@ protects you from accidentally updating an
-- 'Opaleye.Table.optionalTableField' with @Nothing@ (i.e. SQL
-- @DEFAULT@).  See 'uUpdateWith'.
--
-- @uUpdateWith = updateEasy (\\... -> ...)@
updateEasy :: D.Default Updater fieldsR fieldsW
           => (fieldsR -> fieldsR)
           -- ^ ͘
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
           -- ^ ͘
           -> MI.Returning fieldsR [haskells]
rReturning = rReturningExplicit D.def

-- | Like 'rReturning' but with better inference properties.  On the
-- other hand the mapping from SQL fields to Haskell types is less
-- flexible.
rReturningI :: D.Default (Inferrable RS.FromFields) fields haskells
            => (fieldsR -> fields)
            -- ^ ͘
            -> MI.Returning fieldsR [haskells]
rReturningI = rReturningExplicit (runInferrable D.def)

-- | Return a function of the inserted or updated rows.  Explicit
-- version.  You probably just want to use 'rReturning' instead.
rReturningExplicit :: RS.FromFields fields haskells
                   -- ^ ͘
                   -> (fieldsR -> fields)
                   -- ^
                   -> MI.Returning fieldsR [haskells]
rReturningExplicit = MI.ReturningExplicit

-- * Deprecated versions

runInsertMany' :: Maybe HSql.OnConflict
               -> PGS.Connection
               -> TI.Table columnsW columnsR
               -> [columnsW]
               -> IO Int64
runInsertMany' oc conn t columns =
  case NEL.nonEmpty columns of
    -- Inserting the empty list is just the same as returning 0
    Nothing       -> return 0
    Just columns' -> (PGS.execute_ conn . fromString .:. MI.arrangeInsertManySql)
                         t columns' oc

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
  where IRQ.FromFields u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tableColumnsView (TI.tableColumns t)

-- | Skip the conflicting row, leaving the existing row unchanged.
--
-- @
-- iOnConflict = Just doNothing
-- @
doNothing :: HSql.OnConflict
doNothing = HSql.DoNothing

-- | Replace every column with the corresponding @EXCLUDED@ value on
-- conflict.  This is the most common upsert pattern.  Equivalent to
-- @'doUpdateEasy' table conflictTarget id@.
--
-- @
-- -- data Stock' a b = Stock { sku :: a, quantity :: b }
-- -- \$(makeAdaptorAndInstance \"pStock\" \'\'Stock\')
-- -- type Stock = Stock' (Field SqlText) (Field SqlInt4)
-- --
-- -- stockTable :: Table Stock Stock
-- -- stockTable = table "stock" $ pStock Stock
-- --   { sku      = requiredTableField "sku"
-- --   , quantity = requiredTableField "quantity"
-- --   }
-- -- On conflict: both sku and quantity are replaced with the new row's values.
--
-- iOnConflict = Just (doUpdateAll stockTable sku)
-- @
doUpdateAll :: ( D.Default U.Unpackspec conflictCols conflictCols
               , D.Default U.Unpackspec fieldsR fieldsR
               , D.Default MI.Updater fieldsR fieldsW
               )
            => T.Table fieldsW fieldsR
            -> (fieldsR -> conflictCols)
            -- ^ Columns forming the conflict target
            -> HSql.OnConflict
doUpdateAll table conflictTarget = doUpdateEasy table conflictTarget id

-- | Build an @ON CONFLICT (...) DO UPDATE SET@ conflict action.
-- The update function receives the @EXCLUDED@ pseudo-row and returns
-- the new column values.  Optional columns (created with
-- 'Opaleye.Table.optionalTableField') must be wrapped in 'Just';
-- see 'doUpdateEasy' to avoid this.
--
-- @
-- -- data Stock' a b c = Stock { sku :: a, quantity :: b, reserved :: c }
-- -- \$(makeAdaptorAndInstance \"pStock\" \'\'Stock\')
-- -- type Stock  = Stock' (Field SqlText) (Field SqlInt4) (Field SqlInt4)
-- -- type StockW = Stock' (Field SqlText) (Maybe (Field SqlInt4)) (Maybe (Field SqlInt4))
-- --
-- -- stockTable :: Table StockW Stock
-- -- stockTable = table "stock" $ pStock Stock
-- --   { sku      = requiredTableField "sku"
-- --   , quantity = optionalTableField "quantity"
-- --   , reserved = optionalTableField "reserved"
-- --   }
-- -- On conflict: sku unchanged, quantity updated, reserved reset to DEFAULT.
--
-- iOnConflict = Just (doUpdate stockTable sku
--     (\\excl -> Stock
--         { sku      = sku excl       -- unchanged (equals conflict key)
--         , quantity = Just (quantity excl)  -- updated (Just required)
--         , reserved = Nothing               -- reset to column DEFAULT
--         }))
-- @
doUpdate :: ( D.Default U.Unpackspec conflictCols conflictCols
            , D.Default U.Unpackspec fieldsR fieldsR
            )
         => T.Table fieldsW fieldsR
         -> (fieldsR -> conflictCols)
         -- ^ Columns forming the conflict target
         -> (fieldsR -> fieldsW)
         -- ^ Update function; receives the @EXCLUDED@ pseudo-row
         -> HSql.OnConflict
doUpdate = MI.arrangeDoUpdate D.def D.def

-- | Like 'doUpdate' but optional columns do not need to be wrapped in
-- 'Just'.
--
-- @
-- -- (same stockTable as 'doUpdate' above)
-- -- On conflict: sku unchanged, quantity updated, reserved reset to DEFAULT.
-- -- No Just wrapping needed — compare with 'doUpdate'.
--
-- iOnConflict = Just (doUpdateEasy stockTable sku
--     (\\excl -> excl
--         { quantity = quantity excl  -- updated (no Just needed)
--         , reserved = reserved excl  -- updated (no Just needed)
--         }))
-- @
doUpdateEasy :: ( D.Default U.Unpackspec conflictCols conflictCols
                , D.Default U.Unpackspec fieldsR fieldsR
                , D.Default MI.Updater fieldsR fieldsW
                )
             => T.Table fieldsW fieldsR
             -> (fieldsR -> conflictCols)
             -- ^ Columns forming the conflict target
             -> (fieldsR -> fieldsR)
             -- ^ Update function; receives the @EXCLUDED@ pseudo-row
             -> HSql.OnConflict
doUpdateEasy table conflictTarget updateFn =
  doUpdate table conflictTarget (u' . updateFn)
  where Updater u' = D.def
