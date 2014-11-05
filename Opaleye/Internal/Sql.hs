module Opaleye.Internal.Sql where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Database.HaskellDB.PrimQuery as HP
import qualified Database.HaskellDB.Sql as S
import qualified Database.HaskellDB.Sql.Default as SD
import qualified Database.HaskellDB.Sql.Generate as SG

import qualified Data.Maybe as M

type SqlQueryGenerator = PQ.PrimQueryFold S.SqlSelect

sqlQueryGenerator :: SqlQueryGenerator
sqlQueryGenerator = (baseTable, product, aggregate, order, limit)

sqlExpr :: HP.PrimExpr -> S.SqlExpr
sqlExpr = SG.sqlExpr SD.defaultSqlGenerator

baseTable :: String -> [(PQ.Symbol, String)] -> S.SqlSelect
baseTable name columns =
  S.newSelect { S.attrs = map (\(x, y) -> (x, S.ColumnSqlExpr y)) columns
              , S.tables = [("", S.SqlTable name)] }

product :: [S.SqlSelect] -> [HP.PrimExpr] -> S.SqlSelect
product ss pes = S.newSelect { S.tables = map (\s -> ("", s)) ss
                             , S.criteria = map sqlExpr pes }

aggregate :: [(PQ.Symbol, Maybe HP.AggrOp, HP.PrimExpr)] -> S.SqlSelect -> S.SqlSelect
aggregate aggrs s = S.newSelect { S.attrs = attrs
                                , S.tables = [("", s)]
                                , S.groupby = groupBy' }
  where groupBy = (map (\(x, _, y) -> (x, sqlExpr y))
                   . filter (\(_, x, _) -> M.isNothing x)) aggrs
        attrs = (map (\(x, aggrOp, pe) -> (x, sqlExpr (maybe id HP.AggrExpr aggrOp pe)))) aggrs

        groupBy' = case groupBy of [] -> Nothing
                                   xs -> Just (S.Columns groupBy)

order :: [HP.OrderExpr] -> S.SqlSelect -> S.SqlSelect
order oes s = S.newSelect { S.tables = [("", s)]
                          , S.orderby = map (SD.toSqlOrder SD.defaultSqlGenerator) oes }

limit :: PQ.LimitOp -> S.SqlSelect -> S.SqlSelect
limit lo s = case lo of
  PQ.LimitOp n         -> S.newSelect { S.tables = [("", s)]
                                      , S.extra = ["LIMIT " ++ show n] }
  PQ.OffsetOp n        -> S.newSelect { S.tables = [("", s)]
                                      , S.extra = ["OFFSET " ++ show n] }
  PQ.LimitOffsetOp l o -> S.newSelect { S.tables = [("", s)]
                                      , S.extra = ["LIMIT " ++ show l
                                                  ,"OFFSET " ++ show o] }
