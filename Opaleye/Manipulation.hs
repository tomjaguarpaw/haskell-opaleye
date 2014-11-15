module Opaleye.Manipulation where

import qualified Opaleye.Internal.Sql as S
import qualified Opaleye.Table as T
import qualified Opaleye.Internal.Table as TI
import           Opaleye.Column (Column(Column))

import qualified Database.HaskellDB.Sql as HSql
import qualified Database.HaskellDB.Sql.Print as HPrint

import qualified Database.PostgreSQL.Simple as PG

import           Data.Int (Int64)
import           Data.String (fromString)

infixr 8 .:

(.:) :: (r -> z) -> (a -> b -> r) -> a -> b -> z
(.:) f g x y = f (g x y)

infixr 8 .:.

(.:.) :: (r -> z) -> (a -> b -> c -> r) -> a -> b -> c -> z
(.:.) f g a b c = f (g a b c)

arrangeInsert :: T.Table columns a -> columns -> HSql.SqlInsert
arrangeInsert (T.Table tableName (TI.TableProperties writer _)) columns = insert
  where outColumns = TI.runWriter writer columns
        outColumnNames = map snd outColumns
        outColumnSqlExprs = map (S.sqlExpr . fst) outColumns
        insert = HSql.SqlInsert tableName outColumnNames outColumnSqlExprs

arrangeInsertSql :: T.Table columns a -> columns -> String
arrangeInsertSql = show . HPrint.ppInsert .: arrangeInsert

runInsert :: PG.Connection -> T.Table columns columns' -> columns -> IO Int64
runInsert conn = PG.execute_ conn . fromString .: arrangeInsertSql

arrangeUpdate :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> HSql.SqlUpdate
arrangeUpdate (TI.Table tableName (TI.TableProperties writer (TI.View tableCols)))
              update cond =
  HSql.SqlUpdate tableName (update' tableCols) [S.sqlExpr condExpr]
  where update' = map (\(x, y) -> (y, S.sqlExpr x))
                   . TI.runWriter writer
                   . update
        Column condExpr = cond tableCols

arrangeUpdateSql :: T.Table columnsW columnsR
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:. arrangeUpdate

runUpdate :: PG.Connection -> T.Table columnsW columnsR
          -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
          -> IO Int64
runUpdate conn = PG.execute_ conn . fromString .:. arrangeUpdateSql

arrangeDelete :: T.Table a columnsR -> (columnsR -> Column Bool) -> HSql.SqlDelete
arrangeDelete (TI.Table tableName (TI.TableProperties _ (TI.View tableCols)))
              cond =
  HSql.SqlDelete tableName [S.sqlExpr condExpr]
  where Column condExpr = cond tableCols

arrangeDeleteSql :: T.Table a columnsR -> (columnsR -> Column Bool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

runDelete :: PG.Connection -> T.Table a columnsR -> (columnsR -> Column Bool)
          -> IO Int64
runDelete conn = PG.execute_ conn . fromString .: arrangeDeleteSql
