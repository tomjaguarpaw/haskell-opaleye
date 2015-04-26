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
import           Opaleye.Internal.Helpers ((.:), (.:.), (.::))
import qualified Opaleye.Internal.Unpackspec as U
import           Opaleye.PGTypes (PGBool)

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
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
arrangeInsert (T.Table tableName (TI.TableProperties writer _)) columns = insert
  where outColumns' = (map (\(x, y) -> (y, x))
                       . TI.runWriter writer) columns
        insert = SG.sqlInsert SD.defaultSqlGenerator tableName outColumns'

arrangeInsertSql :: T.Table columns a -> columns -> String
arrangeInsertSql = show . HPrint.ppInsert .: arrangeInsert

runInsert :: PGS.Connection -> T.Table columns columns' -> columns -> IO Int64
runInsert conn = PGS.execute_ conn . fromString .: arrangeInsertSql

arrangeInsertMany :: T.Table columns a -> NEL.NonEmpty columns -> HSql.SqlInsert
arrangeInsertMany (T.Table tableName (TI.TableProperties writer _)) columns = insert
  where columnNames = TI.runWriterColumnNames writer (NEL.head columns)
        columnExprs = fmap (TI.runWriterPrimExprs writer) columns
        insert = SG.sqlInsertMany SD.defaultSqlGenerator
                      tableName columnNames columnExprs

arrangeInsertManySql :: T.Table columns a -> NEL.NonEmpty columns -> String
arrangeInsertManySql = show . HPrint.ppInsert .: arrangeInsertMany

runInsertMany :: PGS.Connection
              -> T.Table columns columns'
              -> NEL.NonEmpty columns
              -> IO Int64
runInsertMany conn = PGS.execute_ conn . fromString .: arrangeInsertManySql

arrangeUpdate :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> HSql.SqlUpdate
arrangeUpdate (TI.Table tableName (TI.TableProperties writer (TI.View tableCols)))
              update cond =
  SG.sqlUpdate SD.defaultSqlGenerator tableName [condExpr] (update' tableCols)
  where update' = map (\(x, y) -> (y, x))
                   . TI.runWriter writer
                   . update
        Column condExpr = cond tableCols

arrangeUpdateSql :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:. arrangeUpdate

runUpdate :: PGS.Connection -> T.Table columnsW columnsR
          -> (columnsR -> columnsW) -> (columnsR -> Column PGBool)
          -> IO Int64
runUpdate conn = PGS.execute_ conn . fromString .:. arrangeUpdateSql

arrangeDelete :: T.Table a columnsR -> (columnsR -> Column PGBool) -> HSql.SqlDelete
arrangeDelete (TI.Table tableName (TI.TableProperties _ (TI.View tableCols)))
              cond =
  SG.sqlDelete SD.defaultSqlGenerator tableName [condExpr]
  where Column condExpr = cond tableCols

arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Column PGBool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

runDelete :: PGS.Connection -> T.Table a columnsR -> (columnsR -> Column PGBool)
          -> IO Int64
runDelete conn = PGS.execute_ conn . fromString .: arrangeDeleteSql

arrangeInsertReturning :: U.Unpackspec returned returned
                       -> T.Table columnsW columnsR
                       -> columnsW
                       -> (columnsR -> returned)
                       -> Sql.Returning HSql.SqlInsert
arrangeInsertReturning unpackspec table columns returningf =
  Sql.Returning insert returningSEs
  where insert = arrangeInsert table columns
        TI.Table _ (TI.TableProperties _ (TI.View columnsR)) = table
        returning = returningf columnsR
        -- TODO: duplication with runQueryArrUnpack
        f pe = ([pe], pe)
        returningPEs :: [HPQ.PrimExpr]
        (returningPEs, _) = U.runUnpackspec unpackspec f returning
        returningSEs = map Sql.sqlExpr returningPEs

arrangeInsertReturningSql :: U.Unpackspec returned returned
                          -> T.Table columnsW columnsR
                          -> columnsW
                          -> (columnsR -> returned)
                          -> String
arrangeInsertReturningSql = show
                            . Print.ppInsertReturning
                            .:: arrangeInsertReturning

runInsertReturningExplicit :: RQ.QueryRunner returned haskells
                           -> U.Unpackspec returned returned
                           -> PGS.Connection
                           -> T.Table columnsW columnsR
                           -> columnsW
                           -> (columnsR -> returned)
                           -> IO [haskells]
runInsertReturningExplicit qr u conn = PGS.queryWith_ rowParser conn
                                       . fromString
                                       .:. arrangeInsertReturningSql u
  where IRQ.QueryRunner _ rowParser = qr

-- | @runInsertReturning@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runInsertReturning@.
runInsertReturning :: (D.Default RQ.QueryRunner returned haskells,
                       D.Default U.Unpackspec returned returned)
                      => PGS.Connection
                      -> T.Table columnsW columnsR
                      -> columnsW
                      -> (columnsR -> returned)
                      -> IO [haskells]
runInsertReturning = runInsertReturningExplicit D.def D.def
