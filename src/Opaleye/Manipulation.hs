{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Manipulation (module Opaleye.Manipulation,
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

arrangeInsert :: T.Table columns a -> columns -> HSql.SqlInsert
arrangeInsert t c = arrangeInsertMany t (return c)

arrangeInsertSql :: T.Table columns a -> columns -> String
arrangeInsertSql = Print.renderDoc . HPrint.ppInsert .: arrangeInsert

runInsert :: PGS.Connection -> T.Table columns columns' -> columns -> IO Int64
runInsert conn = PGS.execute_ conn . fromString .: arrangeInsertSql

arrangeInsertMany :: T.Table columns a -> NEL.NonEmpty columns -> HSql.SqlInsert
arrangeInsertMany table columns = insert
  where writer = TI.tablePropertiesWriter (TI.tableProperties table)
        (columnExprs, columnNames) = TI.runWriter' writer columns
        insert = SG.sqlInsert SD.defaultSqlGenerator
                      (PQ.tiToSqlTable (TI.tableIdentifier table))
                      columnNames columnExprs

arrangeInsertManySql :: T.Table columns a -> NEL.NonEmpty columns -> String
arrangeInsertManySql = Print.renderDoc . HPrint.ppInsert .: arrangeInsertMany

runInsertMany :: PGS.Connection
              -> T.Table columns columns'
              -> [columns]
              -> IO Int64
runInsertMany conn table columns = case NEL.nonEmpty columns of
  -- Inserting the empty list is just the same as returning 0
  Nothing       -> return 0
  Just columns' -> (PGS.execute_ conn . fromString .: arrangeInsertManySql) table columns'

arrangeUpdate :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> HSql.SqlUpdate
arrangeUpdate table update cond =
  SG.sqlUpdate SD.defaultSqlGenerator
               (PQ.tiToSqlTable (TI.tableIdentifier table))
               [condExpr] (update' tableCols)
  where TI.TableProperties writer (TI.View tableCols) = TI.tableProperties table
        update' = map (\(x, y) -> (y, x)) . TI.runWriter writer . update
        Column condExpr = cond tableCols

arrangeUpdateSql :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> String
arrangeUpdateSql = Print.renderDoc . HPrint.ppUpdate .:. arrangeUpdate

runUpdate :: PGS.Connection -> T.Table columnsW columnsR
          -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
          -> IO Int64
runUpdate conn = PGS.execute_ conn . fromString .:. arrangeUpdateSql

arrangeDelete :: T.Table a columnsR -> (columnsR -> Column PGBool) -> HSql.SqlDelete
arrangeDelete table cond =
  SG.sqlDelete SD.defaultSqlGenerator (PQ.tiToSqlTable (TI.tableIdentifier table)) [condExpr]
  where Column condExpr = cond tableCols
        TI.View tableCols = TI.tablePropertiesView (TI.tableProperties table)

arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Column PGBool) -> String
arrangeDeleteSql = Print.renderDoc . HPrint.ppDelete .: arrangeDelete

runDelete :: PGS.Connection -> T.Table a columnsR -> (columnsR -> Column PGBool)
          -> IO Int64
runDelete conn = PGS.execute_ conn . fromString .: arrangeDeleteSql

arrangeInsertReturning :: U.Unpackspec returned ignored
                       -> T.Table columnsW columnsR
                       -> columnsW
                       -> (columnsR -> returned)
                       -> Sql.Returning HSql.SqlInsert
arrangeInsertReturning unpackspec table columns returningf =
  Sql.Returning insert returningSEs
  where insert = arrangeInsert table columns
        TI.View columnsR = TI.tablePropertiesView (TI.tableProperties table)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

arrangeInsertReturningSql :: U.Unpackspec returned ignored
                          -> T.Table columnsW columnsR
                          -> columnsW
                          -> (columnsR -> returned)
                          -> String
arrangeInsertReturningSql = Print.renderDoc
                            . Print.ppInsertReturning
                            .:: arrangeInsertReturning

runInsertReturningExplicit :: RQ.QueryRunner returned haskells
                            -> PGS.Connection
                            -> T.Table columnsW columnsR
                            -> columnsW
                            -> (columnsR -> returned)
                            -> IO [haskells]
runInsertReturningExplicit qr conn t w r = PGS.queryWith_ parser conn
                                             (fromString
                                             (arrangeInsertReturningSql u t w r))
  where IRQ.QueryRunner u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tablePropertiesView (TI.tableProperties t)
        -- This method of getting hold of the return type feels a bit
        -- suspect.  I haven't checked it for validity.

-- | @runInsertReturning@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runInsertReturning@.
runInsertReturning :: (D.Default RQ.QueryRunner returned haskells)
                      => PGS.Connection
                      -> T.Table columnsW columnsR
                      -> columnsW
                      -> (columnsR -> returned)
                      -> IO [haskells]
runInsertReturning = runInsertReturningExplicit D.def

arrangeUpdateReturning :: U.Unpackspec returned ignored
                       -> T.Table columnsW columnsR
                       -> (columnsR -> columnsW)
                       -> (columnsR -> Column PGBool)
                       -> (columnsR -> returned)
                       -> Sql.Returning HSql.SqlUpdate
arrangeUpdateReturning unpackspec table updatef cond returningf =
  Sql.Returning update returningSEs
  where update = arrangeUpdate table updatef cond
        TI.View columnsR = TI.tablePropertiesView (TI.tableProperties table)
        returningPEs = U.collectPEs unpackspec (returningf columnsR)
        returningSEs = Sql.ensureColumnsGen id (map Sql.sqlExpr returningPEs)

arrangeUpdateReturningSql :: U.Unpackspec returned ignored
                       -> T.Table columnsW columnsR
                       -> (columnsR -> columnsW)
                       -> (columnsR -> Column PGBool)
                       -> (columnsR -> returned)
                       -> String
arrangeUpdateReturningSql = Print.renderDoc
                            . Print.ppUpdateReturning
                            .::. arrangeUpdateReturning

runUpdateReturningExplicit :: RQ.QueryRunner returned haskells
                           -> PGS.Connection
                           -> T.Table columnsW columnsR
                           -> (columnsR -> columnsW)
                           -> (columnsR -> Column PGBool)
                           -> (columnsR -> returned)
                           -> IO [haskells]
runUpdateReturningExplicit qr conn t update cond r =
  PGS.queryWith_ parser conn
                 (fromString (arrangeUpdateReturningSql u t update cond r))
  where IRQ.QueryRunner u _ _ = qr
        parser = IRQ.prepareRowParser qr (r v)
        TI.View v = TI.tablePropertiesView (TI.tableProperties t)

runUpdateReturning :: (D.Default RQ.QueryRunner returned haskells)
                      => PGS.Connection
                      -> T.Table columnsW columnsR
                      -> (columnsR -> columnsW)
                      -> (columnsR -> Column PGBool)
                      -> (columnsR -> returned)
                      -> IO [haskells]
runUpdateReturning = runUpdateReturningExplicit D.def
