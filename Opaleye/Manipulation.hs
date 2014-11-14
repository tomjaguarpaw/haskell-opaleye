module Opaleye.Manipulation where

import qualified Opaleye.Internal.Sql as S
import qualified Opaleye.Table as T

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
