module Opaleye.Manipulation where

import qualified Opaleye.Internal.Sql as S
import qualified Opaleye.Table as T
import           Opaleye.Column (Column(Column))

import qualified Database.HaskellDB.Sql as HSql
import qualified Database.HaskellDB.Sql.Print as HPrint

import qualified Database.PostgreSQL.Simple as PG

import           Data.Int (Int64)
import           Data.String (fromString)

infixr 8 .:

(.:) :: (r -> z) -> (a -> b -> r) -> a -> b -> z
(.:) f g x y = f (g x y)

infixr 8 .::

(.::) :: (r -> z) -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> z
(.::) f g a b c d = f (g a b c d)

arrangeInsert :: T.Writeable columns columns' -> columns -> HSql.SqlInsert
arrangeInsert (T.Writeable tableName writer) columns = insert
  where outColumns = T.runWriter writer columns
        outColumnNames = map snd outColumns
        outColumnSqlExprs = map (S.sqlExpr . fst) outColumns
        insert = HSql.SqlInsert tableName outColumnNames outColumnSqlExprs

arrangeInsertSql :: T.Writeable columns columns' -> columns -> String
arrangeInsertSql = show . HPrint.ppInsert .: arrangeInsert

runInsert :: PG.Connection -> T.Writeable columns columns' -> columns -> IO Int64
runInsert conn = PG.execute_ conn . fromString .: arrangeInsertSql

arrangeUpdate :: T.Table columnsR -> T.Writeable columnsW columns'
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> HSql.SqlUpdate
arrangeUpdate (T.Table tableName tableCols) (T.Writeable _ writer) update cond =
  HSql.SqlUpdate tableName (update' tableCols) [S.sqlExpr condExpr]
  where update' = map (\(x, y) -> (y, S.sqlExpr x))
                   . T.runWriter writer
                   . update
        Column condExpr = cond tableCols

arrangeUpdateSql :: T.Table columnsR -> T.Writeable columnsW columns'
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> String
arrangeUpdateSql = show . HPrint.ppUpdate .:: arrangeUpdate

runUpdate :: PG.Connection -> T.Table columnsR -> T.Writeable columnsW columns'
          -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
          -> IO Int64
runUpdate conn = PG.execute_ conn . fromString .:: arrangeUpdateSql

arrangeDelete :: T.Table columnsR -> (columnsR -> Column Bool) -> HSql.SqlDelete
arrangeDelete (T.Table tableName tableCols) cond =
  HSql.SqlDelete tableName [S.sqlExpr condExpr]
  where Column condExpr = cond tableCols

arrangeDeleteSql :: T.Table columnsR -> (columnsR -> Column Bool) -> String
arrangeDeleteSql = show . HPrint.ppDelete .: arrangeDelete

runDelete :: PG.Connection -> T.Table columnsR -> (columnsR -> Column Bool)
          -> IO Int64
runDelete conn = PG.execute_ conn . fromString .: arrangeDeleteSql
