module Opaleye.Internal.Sql where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Database.HaskellDB.PrimQuery as HP
import qualified Database.HaskellDB.Sql as S
import qualified Database.HaskellDB.Sql.Default as SD
import qualified Database.HaskellDB.Sql.Generate as SG
import qualified Database.HaskellDB.Sql.Print as Pr

import qualified Data.Maybe as M

import qualified Text.PrettyPrint.HughesPJ as HPJ

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

-- FIXME: I doubt this is going to work if ss is empty
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
                                   _  -> Just (S.Columns groupBy)

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
