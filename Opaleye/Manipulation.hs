module Opaleye.Manipulation where

import qualified Opaleye.Internal.Sql as S
import qualified Opaleye.Table as T
import           Opaleye.Column (Column(Column))

import qualified Database.HaskellDB.Sql as HSql
import qualified Database.HaskellDB.Sql.Print as HPrint

import qualified Database.PostgreSQL.Simple as PG

import           Data.Int (Int64)
import           Data.String (fromString)

arrangeInsert :: T.Writeable columns columns' -> columns -> HSql.SqlInsert
arrangeInsert (T.Writeable tableName writer) columns = insert
  where outColumns = T.runWriter writer columns
        outColumnNames = map snd outColumns
        outColumnSqlExprs = map (S.sqlExpr . fst) outColumns
        insert = HSql.SqlInsert tableName outColumnNames outColumnSqlExprs

-- TODO: use .:
arrangeInsertSql :: T.Writeable columns columns' -> columns -> String
arrangeInsertSql w c = (show . HPrint.ppInsert) (arrangeInsert w c)

runInsert :: PG.Connection -> T.Writeable columns columns' -> columns -> IO Int64
runInsert conn w c = (PG.execute_ conn . fromString) (arrangeInsertSql w c)

arrangeUpdate :: T.Table columnsR -> T.Writeable columnsW columns'
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> HSql.SqlUpdate
arrangeUpdate (T.Table tableName tableCols) (T.Writeable _ writer) update cond =
  HSql.SqlUpdate tableName (update' tableCols) [S.sqlExpr condExpr]
  where update' = map (\(x, y) -> (y, S.sqlExpr x))
                   . T.runWriter writer
                   . update
        Column condExpr = cond tableCols

-- TODO: use .::
arrangeUpdateSql :: T.Table columnsR -> T.Writeable columnsW columns'
              -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
              -> String
arrangeUpdateSql t w u c = (show . HPrint.ppUpdate) (arrangeUpdate t w u c)

runUpdate :: PG.Connection -> T.Table columnsR -> T.Writeable columnsW columns'
          -> (columnsR -> columnsW) -> (columnsR -> Column Bool)
          -> IO Int64
runUpdate conn t w u c = (PG.execute_ conn . fromString)
                         (arrangeUpdateSql t w u c)

arrangeDelete :: T.Table columnsR -> (columnsR -> Column Bool)
              -> HSql.SqlDelete
arrangeDelete (T.Table tableName tableCols) cond =
  HSql.SqlDelete tableName [S.sqlExpr condExpr]
  where Column condExpr = cond tableCols

-- TODO: use .:
arrangeDeleteSql :: T.Table columnsR -> (columnsR -> Column Bool)
                    -> String
arrangeDeleteSql t c = (show . HPrint.ppDelete) (arrangeDelete t c)

runDelete :: PG.Connection -> T.Table columnsR -> (columnsR -> Column Bool)
          -> IO Int64
runDelete conn t c = (PG.execute_ conn . fromString) (arrangeDeleteSql t c)
