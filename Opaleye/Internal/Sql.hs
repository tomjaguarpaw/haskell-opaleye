module Opaleye.Internal.Sql where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.NEList as NE

import qualified Database.HaskellDB.PrimQuery as HP
import qualified Database.HaskellDB.Sql as S
import qualified Database.HaskellDB.Sql.Default as SD
import qualified Database.HaskellDB.Sql.Generate as SG

import qualified Data.Maybe as M

data Select = SelectFrom From
            | Table S.SqlTable
            | SelectJoin Join
            | SelectValues Values
            deriving Show

data From = From {
  attrs     :: [(S.SqlExpr, Maybe S.SqlColumn)],
  tables    :: [Select],
  criteria  :: [S.SqlExpr],
  groupBy   :: [S.SqlExpr],
  orderBy   :: [(S.SqlExpr, S.SqlOrder)],
  limit     :: Maybe Int,
  offset    :: Maybe Int
  }
          deriving Show

data Join = Join {
  jJoinType   :: JoinType,
  jAttrs      :: [(S.SqlExpr, Maybe S.SqlColumn)],
  jTables     :: (Select, Select),
  jCond       :: S.SqlExpr
  }
                deriving Show

data Values = Values {
  vAttrs  :: [(S.SqlExpr, Maybe S.SqlColumn)],
  vValues :: [[S.SqlExpr]]
} deriving Show

data JoinType = LeftJoin deriving Show
data Distinct = Distinct

data TableName = String


sqlQueryGenerator :: PQ.PrimQueryFold Select
sqlQueryGenerator = (unit, baseTable, product, aggregate, order, limit_, join,
                     values)

sql :: (PQ.PrimQuery, [HP.PrimExpr]) -> Select
sql (pq, pes) = SelectFrom $ newSelect { attrs = makeAttrs pes
                                       , tables = [pqSelect] }
  where pqSelect = PQ.foldPrimQuery sqlQueryGenerator pq
        makeAttrs = flip (zipWith makeAttr) [1..]
        makeAttr pe i = (sqlExpr pe, Just ("result" ++ show (i :: Int)))

unit :: Select
unit = SelectFrom newSelect { attrs  = [(S.ConstSqlExpr "0", Nothing)] }

baseTable :: String -> [(PQ.Symbol, String)] -> Select
baseTable name columns = SelectFrom $
    newSelect { attrs = map (\(sym, col) -> (S.ColumnSqlExpr col, Just sym)) columns
              , tables = [Table name] }

product :: NE.NEList Select -> [HP.PrimExpr] -> Select
product ss pes = SelectFrom $
    newSelect { tables = NE.toList ss
              , criteria = map sqlExpr pes }

aggregate :: [(PQ.Symbol, Maybe HP.AggrOp, HP.PrimExpr)] -> Select -> Select
aggregate aggrs s = SelectFrom $ newSelect { attrs = map attr aggrs
                                           , tables = [s]
                                           , groupBy = groupBy' }
  where groupBy' = (map sqlExpr
                    . map (\(_, _, e) -> e)
                    . filter (\(_, x, _) -> M.isNothing x)) aggrs
        attr (x, aggrOp, pe) = (sqlExpr (aggrExpr aggrOp pe), Just x)

aggrExpr :: Maybe HP.AggrOp -> HP.PrimExpr -> HP.PrimExpr
aggrExpr = maybe id HP.AggrExpr

order :: [HP.OrderExpr] -> Select -> Select
order oes s = SelectFrom $
    newSelect { tables = [s]
              , orderBy = map (SD.toSqlOrder SD.defaultSqlGenerator) oes }

limit_ :: PQ.LimitOp -> Select -> Select
limit_ lo s = SelectFrom $ newSelect { tables = [s]
                                     , limit = limit'
                                     , offset = offset' }
  where (limit', offset') = case lo of
          PQ.LimitOp n         -> (Just n, Nothing)
          PQ.OffsetOp n        -> (Nothing, Just n)
          PQ.LimitOffsetOp l o -> (Just l, Just o)

join :: PQ.JoinType -> [(PQ.Symbol, HP.PrimExpr)] -> HP.PrimExpr -> Select -> Select
     -> Select
join j columns cond s1 s2 = SelectJoin Join { jJoinType = joinType j
                                            , jAttrs = mkAttrs columns
                                            , jTables = (s1, s2)
                                            , jCond = sqlExpr cond }
  where mkAttrs = map (\(sym, pe) -> (sqlExpr pe, Just sym))

values :: [PQ.Symbol] -> [[HP.PrimExpr]] -> Select
values columns pes = SelectValues Values { vAttrs  = mkColumns columns
                                         , vValues = (map . map) sqlExpr pes }
  where mkColumns = zipWith (\i column -> ((sqlExpr . HP.AttrExpr) ("column" ++ show (i::Int)),
                                           Just column)) [1..]

joinType :: PQ.JoinType -> JoinType
joinType PQ.LeftJoin = LeftJoin

newSelect :: From
newSelect = From {
  attrs     = [],
  tables    = [],
  criteria  = [],
  groupBy   = [],
  orderBy   = [],
  limit     = Nothing,
  offset    = Nothing
  }

sqlExpr :: HP.PrimExpr -> S.SqlExpr
sqlExpr = SG.sqlExpr SD.defaultSqlGenerator
