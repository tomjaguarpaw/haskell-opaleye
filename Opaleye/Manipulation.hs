module Opaleye.Manipulation where

import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Table as T
import qualified Opaleye.Internal.Table as TI
import           Opaleye.Internal.Column (Column(Column))
import           Opaleye.Internal.Helpers ((.:), (.:.))

import qualified Database.HaskellDB.Sql as HSql
import qualified Database.HaskellDB.Sql.Print as HPrint

import qualified Database.PostgreSQL.Simple as PGS

import           Data.Int (Int64)
import           Data.String (fromString)

arrangeInsert :: T.Table columns a -> columns -> HSql.SqlInsert
arrangeInsert (T.Table tableName (TI.TableProperties writer _)) columns = insert
  where outColumns = TI.runWriter writer columns
        outColumnNames = map snd outColumns
        outColumnSqlExprs = map (Sql.sqlExpr . fst) outColumns
        insert = HSql.SqlInsert tableName outColumnNames outColumnSqlExprs

arrangeInsertSql :: T.Table columns a -> columns -> String
arrangeInsertSql = show . HPrint.ppInsert .: arrangeInsert

runInsert :: PGS.Connection -> T.Table columns columns' -> columns -> IO Int64
runInsert conn = PGS.execute_ conn . fromString .: arrangeInsertSql

arrangeUpdate :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> HSql.SqlUpdate
arrangeUpdate (TI.Table tableName (TI.TableProperties writer (TI.View tableCols)))
              update cond =
  HSql.SqlUpdate tableName (update' tableCols) [Sql.sqlExpr condExpr]
  where update' = map (\(x, y) -> (y, Sql.sqlExpr x))
                   . TI.runWriter writer
                   . update
        Column condExpr = cond tableCols

arrangeUpdateSql :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:. arrangeUpdate

runUpdate :: PGS.Connection -> T.Table columnsW columnsR
          -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
          -> IO Int64
runUpdate conn = PGS.execute_ conn . fromString .:. arrangeUpdateSql

arrangeDelete :: T.Table a columnsR -> (columnsR -> Column Bool) -> HSql.SqlDelete
arrangeDelete (TI.Table tableName (TI.TableProperties _ (TI.View tableCols)))
              cond =
  HSql.SqlDelete tableName [Sql.sqlExpr condExpr]
  where Column condExpr = cond tableCols

arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Column Bool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

runDelete :: PGS.Connection -> T.Table a columnsR -> (columnsR -> Column Bool)
          -> IO Int64
runDelete conn = PGS.execute_ conn . fromString .: arrangeDeleteSql
