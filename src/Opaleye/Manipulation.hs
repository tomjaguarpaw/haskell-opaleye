{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Manipulation (module Opaleye.Manipulation,
                             U.Unpackspec) where

import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Internal.Print as Print
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

arrangeInsert :: T.Table columns a -> columns -> HSql.SqlInsert
arrangeInsert (T.Table tableName (TI.TableProperties writer _)) columns = insert
  where outColumns' = (map (\(x, y) -> (y, x))
                       . TI.runWriter writer) columns
        insert = SG.sqlInsert SD.defaultSqlGenerator tableName outColumns'

arrangeInsertSql :: T.Table columns a -> columns -> String
arrangeInsertSql = show . HPrint.ppInsert .: arrangeInsert

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

arrangeDelete :: T.Table a columnsR -> (columnsR -> Column PGBool) -> HSql.SqlDelete
arrangeDelete (TI.Table tableName (TI.TableProperties _ (TI.View tableCols)))
              cond =
  SG.sqlDelete SD.defaultSqlGenerator tableName [condExpr]
  where Column condExpr = cond tableCols

arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Column PGBool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

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
