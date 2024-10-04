{-# LANGUAGE GADTs                 #-}

module Opaleye.Internal.Manipulation where

import qualified Control.Applicative as A

import           Opaleye.Internal.Column (Field_(Column), Field)
import qualified Opaleye.Internal.HaskellDB.Sql  as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Default  as SD
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as SG
import qualified Opaleye.Internal.HaskellDB.Sql.Print    as HPrint
import           Opaleye.Internal.Helpers        ((.:), (.:.), (.::.), (.::))
import qualified Opaleye.Internal.PrimQuery      as PQ
import qualified Opaleye.Internal.Print          as Print
import qualified Opaleye.Internal.RunQuery       as IRQ
import qualified Opaleye.RunSelect               as RS
import qualified Opaleye.Internal.Sql            as Sql
import qualified Opaleye.Internal.Table          as TI
import qualified Opaleye.Internal.Unpackspec     as U
import qualified Opaleye.Table                   as T
import           Opaleye.SqlTypes (SqlBool)

import           Data.Int                       (Int64)
import qualified Data.List.NonEmpty              as NEL
import           Data.Profunctor                 (Profunctor, dimap)
import qualified Data.Profunctor.Product         as PP
import qualified Data.Profunctor.Product.Default as D
import           Data.String                     (fromString)

import qualified Database.PostgreSQL.Simple as PGS

-- | Represents a @RETURNING@ statement for a manipulation query.
data Returning fields haskells where
  Count
    :: Returning a Int64
  ReturningExplicit
    :: RS.FromFields b c -> (a -> b) -> Returning a [c]

arrangeInsertMany :: T.Table columns a
                  -> NEL.NonEmpty columns
                  -> Maybe HSql.OnConflict
                  -> HSql.SqlInsert
arrangeInsertMany table columns onConflict = insert
  where writer = TI.tableColumnsWriter (TI.tableColumns table)
        (columnExprs, columnNames) = TI.runWriter' writer columns
        insert = SG.sqlInsert SD.defaultSqlGenerator
                      (PQ.tiToSqlTable (TI.tableIdentifier table))
                      columnNames columnExprs
                      onConflict

arrangeInsertManyReturning :: U.Unpackspec columnsReturned ignored
                           -> T.Table columnsW columnsR
                           -> NEL.NonEmpty columnsW
                           -> (columnsR -> columnsReturned)
                           -> Maybe HSql.OnConflict
                           -> Sql.Returning HSql.SqlInsert
arrangeInsertManyReturning unpackspec table columns returningf onConflict =
  Sql.Returning insert returningSEs
  where insert = arrangeInsertMany table columns onConflict
        TI.View columnsR = TI.tableColumnsView (TI.tableColumns table)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

arrangeInsertManyReturningSql :: U.Unpackspec columnsReturned ignored
                              -> T.Table columnsW columnsR
                              -> NEL.NonEmpty columnsW
                              -> (columnsR -> columnsReturned)
                              -> Maybe HSql.OnConflict
                              -> String
arrangeInsertManyReturningSql =
  show . Print.ppInsertReturning .::. arrangeInsertManyReturning

arrangeInsertManySql :: T.Table columnsW columnsR
                     -> NEL.NonEmpty columnsW
                     -> Maybe HSql.OnConflict
                     -> String
arrangeInsertManySql =
  show . HPrint.ppInsert .:. arrangeInsertMany

runInsertManyReturningExplicit
  :: RS.FromFields columnsReturned haskells
  -> PGS.Connection
  -> T.Table columnsW columnsR
  -> [columnsW]
  -> (columnsR -> columnsReturned)
  -> Maybe HSql.OnConflict
  -> IO [haskells]
runInsertManyReturningExplicit
  qr conn t columns r onConflict =
  case NEL.nonEmpty columns of
    Nothing       -> return []
    Just columns' -> PGS.queryWith_ parser conn
                       (fromString
                        (arrangeInsertManyReturningSql u t columns' r
                                                       onConflict))
  where IRQ.FromFields u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tableColumnsView (TI.tableColumns t)
        -- This method of getting hold of the return type feels a bit
        -- suspect.  I haven't checked it for validity.

newtype Updater a b = Updater (a -> b)

-- { Boilerplate instances

instance Functor (Updater a) where
  fmap f (Updater g) = Updater (fmap f g)

instance A.Applicative (Updater a) where
  pure = Updater . A.pure
  Updater f <*> Updater x = Updater (f A.<*> x)

instance Profunctor Updater where
  dimap f g (Updater h) = Updater (dimap f g h)

instance PP.ProductProfunctor Updater where
  purePP = pure
  (****) = (<*>)

--

instance D.Default Updater (Field_ n a) (Field_ n a) where
  def = Updater id

instance D.Default Updater (Field_ n a) (Maybe (Field_ n a)) where
  def = Updater Just

instance D.Default Updater (Field_ n a) () where
  def = Updater (const ())

arrangeDeleteReturning :: U.Unpackspec columnsReturned ignored
                       -> T.Table columnsW columnsR
                       -> (columnsR -> Field SqlBool)
                       -> (columnsR -> columnsReturned)
                       -> Sql.Returning HSql.SqlDelete
  -- this implementation was copied, it does not make sense yet
arrangeDeleteReturning unpackspec t cond returningf =
  Sql.Returning delete returningSEs
  where delete = arrangeDelete t cond
        TI.View columnsR = TI.tableColumnsView (TI.tableColumns t)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

arrangeDeleteReturningSql :: U.Unpackspec columnsReturned ignored
                          -> T.Table columnsW columnsR
                          -> (columnsR -> Field SqlBool)
                          -> (columnsR -> columnsReturned)
                          -> String
arrangeDeleteReturningSql =
  show . Print.ppDeleteReturning .:: arrangeDeleteReturning


runDeleteReturning :: (D.Default RS.FromFields columnsReturned haskells)
                   => PGS.Connection
                   -- ^
                   -> T.Table a columnsR
                   -- ^ Table to delete rows from
                   -> (columnsR -> Field SqlBool)
                   -- ^ Predicate function @f@ to choose which rows to delete.
                   -- 'runDeleteReturning' will delete rows for which @f@ returns @TRUE@
                   -- and leave unchanged rows for
                   -- which @f@ returns @FALSE@.
                   -> (columnsR -> columnsReturned)
                   -> IO [haskells]
                   -- ^ Returned rows which have been deleted
runDeleteReturning = runDeleteReturningExplicit D.def

runDeleteReturningExplicit :: RS.FromFields columnsReturned haskells
                           -> PGS.Connection
                           -> T.Table a columnsR
                           -> (columnsR -> Field SqlBool)
                           -> (columnsR -> columnsReturned)
                           -> IO [haskells]
runDeleteReturningExplicit qr conn t cond r =
  PGS.queryWith_ parser conn
                 (fromString (arrangeDeleteReturningSql u t cond r))
  where IRQ.FromFields u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tableColumnsView (TI.tableColumns t)

arrangeDelete :: T.Table a columnsR -> (columnsR -> Field SqlBool) -> HSql.SqlDelete
arrangeDelete t cond =
  SG.sqlDelete SD.defaultSqlGenerator (PQ.tiToSqlTable (TI.tableIdentifier t)) [condExpr]
  where Column condExpr = cond tableCols
        TI.View tableCols = TI.tableColumnsView (TI.tableColumns t)

arrangeUpdate :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Field SqlBool)
              -> HSql.SqlUpdate
arrangeUpdate t update cond =
  SG.sqlUpdate SD.defaultSqlGenerator
               (PQ.tiToSqlTable (TI.tableIdentifier t))
               [condExpr] (update' tableCols)
  where TI.TableFields writer (TI.View tableCols) = TI.tableColumns t
        update' = map (\(x, y) -> (y, x)) . TI.runWriter writer . update
        Column condExpr = cond tableCols

arrangeUpdateSql :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Field SqlBool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:. arrangeUpdate

arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Field SqlBool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

arrangeUpdateReturning :: U.Unpackspec columnsReturned ignored
                       -> T.Table columnsW columnsR
                       -> (columnsR -> columnsW)
                       -> (columnsR -> Field SqlBool)
                       -> (columnsR -> columnsReturned)
                       -> Sql.Returning HSql.SqlUpdate
arrangeUpdateReturning unpackspec t updatef cond returningf =
  Sql.Returning update returningSEs
  where update = arrangeUpdate t updatef cond
        TI.View columnsR = TI.tableColumnsView (TI.tableColumns t)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

arrangeUpdateReturningSql :: U.Unpackspec columnsReturned ignored
                          -> T.Table columnsW columnsR
                          -> (columnsR -> columnsW)
                          -> (columnsR -> Field SqlBool)
                          -> (columnsR -> columnsReturned)
                          -> String
arrangeUpdateReturningSql =
  show . Print.ppUpdateReturning .::. arrangeUpdateReturning
