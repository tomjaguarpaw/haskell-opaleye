module Opaleye.Internal.SqlOpaleye where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.NEList as NE
import qualified Opaleye.Internal.Sql as Old

import qualified Database.HaskellDB.PrimQuery as HP
import qualified Database.HaskellDB.Sql as S
import qualified Database.HaskellDB.Sql.Default as SD

import qualified Data.Maybe as M


data Select = SelectFrom From
            | Table S.SqlTable
            | SelectLeftJoin LeftJoin
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

data LeftJoin = LeftJoin {
  jAttrs  :: [(S.SqlExpr, Maybe S.SqlColumn)],
  jTables :: (Select, Select),
  jCond   :: S.SqlExpr
  }
                deriving Show

data Distinct = Distinct

data TableName = String


sqlQueryGenerator :: PQ.PrimQueryFold Select
sqlQueryGenerator = (unit, baseTable, product, aggregate, order, limit_, leftJoin)

sql :: (PQ.PrimQuery, [HP.PrimExpr]) -> Select
sql (pq, pes) = SelectFrom $ newSelect { attrs = makeAttrs pes
                                       , tables = [pqSelect] }
  where pqSelect = PQ.foldPrimQuery sqlQueryGenerator pq
        makeAttrs = flip (zipWith makeAttr) [1..]
        makeAttr pe i = (Old.sqlExpr pe, Just ("result" ++ show (i :: Int)))

unit :: Select
unit = SelectFrom newSelect { attrs  = [(S.ConstSqlExpr "0", Nothing)] }

baseTable :: String -> [(PQ.Symbol, String)] -> Select
baseTable name columns = SelectFrom $
    newSelect { attrs = map (\(sym, col) -> (S.ColumnSqlExpr col, Just sym)) columns
              , tables = [Table name] }

product :: NE.NEList Select -> [HP.PrimExpr] -> Select
product ss pes = SelectFrom $
    newSelect { tables = NE.toList ss
              , criteria = map Old.sqlExpr pes }

aggregate :: [(PQ.Symbol, Maybe HP.AggrOp, HP.PrimExpr)] -> Select -> Select
aggregate aggrs s = SelectFrom $ newSelect { attrs = map attr aggrs
                                           , tables = [s]
                                           , groupBy = groupBy' }
  where groupBy' = (map Old.sqlExpr
                    . map (\(_, _, e) -> e)
                    . filter (\(_, x, _) -> M.isNothing x)) aggrs
        attr (x, aggrOp, pe) = (Old.sqlExpr (Old.aggrExpr aggrOp pe), Just x)

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

leftJoin :: [(PQ.Symbol, HP.PrimExpr)] -> HP.PrimExpr -> Select -> Select -> Select
leftJoin columns cond s1 s2 = SelectLeftJoin LeftJoin { jAttrs = mkAttrs columns
                                                      , jTables = (s1, s2)
                                                      , jCond = Old.sqlExpr cond }
  where mkAttrs = map (\(sym, pe) -> (Old.sqlExpr pe, Just sym))

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
