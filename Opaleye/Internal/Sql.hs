module Opaleye.Internal.Sql where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Default as SD
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as SG

import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as M

data Select = SelectFrom From
            | Table HSql.SqlTable
            | SelectJoin Join
            | SelectValues Values
            | SelectBinary Binary
            deriving Show

data From = From {
  attrs     :: [(HSql.SqlExpr, Maybe HSql.SqlColumn)],
  tables    :: [Select],
  criteria  :: [HSql.SqlExpr],
  groupBy   :: [HSql.SqlExpr],
  orderBy   :: [(HSql.SqlExpr, HSql.SqlOrder)],
  limit     :: Maybe Int,
  offset    :: Maybe Int
  }
          deriving Show

data Join = Join {
  jJoinType   :: JoinType,
  jAttrs      :: [(HSql.SqlExpr, Maybe HSql.SqlColumn)],
  jTables     :: (Select, Select),
  jCond       :: HSql.SqlExpr
  }
                deriving Show

data Values = Values {
  vAttrs  :: [(HSql.SqlExpr, Maybe HSql.SqlColumn)],
  vValues :: [[HSql.SqlExpr]]
} deriving Show

data Binary = Binary {
  bOp :: BinOp,
  bSelect1 :: Select,
  bSelect2 :: Select
} deriving Show

data JoinType = LeftJoin deriving Show
data BinOp = Except | Union | UnionAll deriving Show

data TableName = String

data Returning a = Returning a [HSql.SqlExpr]

sqlQueryGenerator :: PQ.PrimQueryFold Select
sqlQueryGenerator = (unit, baseTable, product, aggregate, order, limit_, join,
                     values, binary)

sql :: (PQ.PrimQuery, [HPQ.PrimExpr]) -> Select
sql (pq, pes) = SelectFrom $ newSelect { attrs = makeAttrs pes
                                       , tables = [pqSelect] }
  where pqSelect = PQ.foldPrimQuery sqlQueryGenerator pq
        makeAttrs = flip (zipWith makeAttr) [1..]
        makeAttr pe i = (sqlExpr pe, Just ("result" ++ show (i :: Int)))

unit :: Select
unit = SelectFrom newSelect { attrs  = [(HSql.ConstSqlExpr "0", Nothing)] }

baseTable :: String -> [(PQ.Symbol, HPQ.PrimExpr)] -> Select
baseTable name columns = SelectFrom $
    newSelect { attrs = map (\(sym, col) -> (sqlExpr col, Just sym)) columns
              , tables = [Table name] }

product :: NEL.NonEmpty Select -> [HPQ.PrimExpr] -> Select
product ss pes = SelectFrom $
    newSelect { tables = NEL.toList ss
              , criteria = map sqlExpr pes }

aggregate :: [(PQ.Symbol, Maybe HPQ.AggrOp, HPQ.PrimExpr)] -> Select -> Select
aggregate aggrs s = SelectFrom $ newSelect { attrs = map attr aggrs
                                           , tables = [s]
                                           , groupBy = groupBy' }
  where groupBy' = (map sqlExpr
                    . map (\(_, _, e) -> e)
                    . filter (\(_, x, _) -> M.isNothing x)) aggrs
        attr (x, aggrOp, pe) = (sqlExpr (aggrExpr aggrOp pe), Just x)

aggrExpr :: Maybe HPQ.AggrOp -> HPQ.PrimExpr -> HPQ.PrimExpr
aggrExpr = maybe id HPQ.AggrExpr

order :: [HPQ.OrderExpr] -> Select -> Select
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

join :: PQ.JoinType -> [(PQ.Symbol, HPQ.PrimExpr)] -> HPQ.PrimExpr -> Select -> Select
     -> Select
join j columns cond s1 s2 = SelectJoin Join { jJoinType = joinType j
                                            , jAttrs = mkAttrs columns
                                            , jTables = (s1, s2)
                                            , jCond = sqlExpr cond }
  where mkAttrs = map (\(sym, pe) -> (sqlExpr pe, Just sym))

values :: [PQ.Symbol] -> [[HPQ.PrimExpr]] -> Select
values columns pes = SelectValues Values { vAttrs  = mkColumns columns
                                         , vValues = (map . map) sqlExpr pes }
  where mkColumns = zipWith (\i column -> ((sqlExpr . HPQ.AttrExpr) ("column" ++ show (i::Int)),
                                           Just column)) [1..]

binary :: PQ.BinOp -> [(PQ.Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))]
       -> (Select, Select) -> Select
binary op pes (select1, select2) = SelectBinary Binary {
  bOp = binOp op,
  bSelect1 = SelectFrom newSelect { attrs = map (mkColumn fst) pes,
                                    tables = [select1] },
  bSelect2 = SelectFrom newSelect { attrs = map (mkColumn snd) pes,
                                    tables = [select2] }
  }
  where mkColumn e (sym, pes') = (sqlExpr (e pes'), Just sym)

joinType :: PQ.JoinType -> JoinType
joinType PQ.LeftJoin = LeftJoin

binOp :: PQ.BinOp -> BinOp
binOp o = case o of
  PQ.Except   -> Except
  PQ.Union    -> Union
  PQ.UnionAll -> UnionAll

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

sqlExpr :: HPQ.PrimExpr -> HSql.SqlExpr
sqlExpr = SG.sqlExpr SD.defaultSqlGenerator
