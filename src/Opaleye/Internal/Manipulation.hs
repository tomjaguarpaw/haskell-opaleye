{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opaleye.Internal.Manipulation where

import qualified Control.Applicative as A

import           Opaleye.Internal.Column (Column)
import qualified Opaleye.Internal.HaskellDB.Sql  as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Default  as SD
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as SG
import qualified Opaleye.Internal.HaskellDB.Sql.Print    as HPrint
import           Opaleye.Internal.Helpers        ((.:.), (.::.))
import qualified Opaleye.Internal.PrimQuery      as PQ
import qualified Opaleye.Internal.Print          as Print
import qualified Opaleye.Internal.RunQuery       as IRQ
import qualified Opaleye.RunQuery                as RQ
import qualified Opaleye.Internal.Sql            as Sql
import qualified Opaleye.Internal.Table          as TI
import qualified Opaleye.Internal.Unpackspec     as U
import qualified Opaleye.Table                   as T

import qualified Data.List.NonEmpty              as NEL
import           Data.Profunctor                 (Profunctor, dimap)
import qualified Data.Profunctor.Product         as PP
import qualified Data.Profunctor.Product.Default as D
import           Data.String                     (fromString)

import qualified Database.PostgreSQL.Simple as PGS

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
  :: RQ.QueryRunner columnsReturned haskells
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
  where IRQ.QueryRunner u _ _ = qr
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
  empty  = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

--

instance D.Default Updater (Column a) (Column a) where
  def = Updater id

instance D.Default Updater (Column a) (Maybe (Column a)) where
  def = Updater Just

