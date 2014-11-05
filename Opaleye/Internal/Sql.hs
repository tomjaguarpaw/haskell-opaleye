module Opaleye.Internal.Sql where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.NEList as NE

import qualified Database.HaskellDB.PrimQuery as HP
import qualified Database.HaskellDB.Sql as S
import qualified Database.HaskellDB.Sql.Default as SD
import qualified Database.HaskellDB.Sql.Generate as SG

import qualified Data.Maybe as M

type SqlQueryGenerator = PQ.PrimQueryFold S.SqlSelect

sqlQueryGenerator :: SqlQueryGenerator
sqlQueryGenerator = (unit, baseTable, product, aggregate, order, limit)

sql :: (PQ.PrimQuery, [HP.PrimExpr]) -> S.SqlSelect
sql (pq, pes) = S.newSelect { S.attrs = attrs
                            , S.tables = [("", pqSelect)] }
  where pqSelect = PQ.foldPrimQuery sqlQueryGenerator pq
        attrs = zipWith (\pe i -> ("result" ++ show i , sqlExpr pe)) pes [1 :: Int ..]

sqlExpr :: HP.PrimExpr -> S.SqlExpr
sqlExpr = SG.sqlExpr SD.defaultSqlGenerator

-- This is a bit dumb
unit :: S.SqlSelect
unit = S.newSelect { S.attrs = [("", S.ConstSqlExpr "0")] }

baseTable :: String -> [(PQ.Symbol, String)] -> S.SqlSelect
baseTable name columns =
  S.newSelect { S.attrs = map (\(x, y) -> (x, S.ColumnSqlExpr y)) columns
              , S.tables = [("", S.SqlTable name)] }

product :: NE.NEList S.SqlSelect -> [HP.PrimExpr] -> S.SqlSelect
product ss pes = S.newSelect { S.tables = (map anonTable . NE.toList) ss
                             , S.criteria = map sqlExpr pes }

aggregate :: [(PQ.Symbol, Maybe HP.AggrOp, HP.PrimExpr)] -> S.SqlSelect -> S.SqlSelect
aggregate aggrs s = S.newSelect { S.attrs = attrs
                                , S.tables = [anonTable s]
                                , S.groupby = groupBy' }
  where groupBy = (map (\(x, _, y) -> (x, sqlExpr y))
                   . filter (\(_, x, _) -> M.isNothing x)) aggrs
        attrs = (map (\(x, aggrOp, pe) -> (x, sqlExpr (maybe id HP.AggrExpr aggrOp pe)))) aggrs

        groupBy' = case groupBy of [] -> Nothing
                                   _  -> Just (S.Columns groupBy)

order :: [HP.OrderExpr] -> S.SqlSelect -> S.SqlSelect
order oes s = S.newSelect { S.tables = [anonTable s]
                          , S.orderby = map (SD.toSqlOrder SD.defaultSqlGenerator) oes }

limit :: PQ.LimitOp -> S.SqlSelect -> S.SqlSelect
limit lo s = S.newSelect { S.tables = [anonTable s]
                         , S.extra = extra }
  where extra = case lo of
          PQ.LimitOp n         -> [limit' n]
          PQ.OffsetOp n        -> [offset n]
          PQ.LimitOffsetOp l o -> [limit' l, offset o]
        limit' n = "LIMIT " ++ show n
        offset n = "OFFSET " ++ show n

anonTable :: S.SqlSelect -> (String, S.SqlSelect)
anonTable s = ("", s)
