module Opaleye.Internal.Sql where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye.Internal.HaskellDB.PrimQuery (Symbol(Symbol))
import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Default as SD
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as SG
import qualified Opaleye.Internal.Tag as T

import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as M

import qualified Control.Arrow as Arr

data Select = SelectFrom From
            | Table HSql.SqlTable
            | SelectJoin Join
            | SelectValues Values
            | SelectBinary Binary
            deriving Show

data SelectAttrs =
    Star
  | SelectAttrs (NEL.NonEmpty (HSql.SqlExpr, Maybe HSql.SqlColumn))
  deriving Show

data From = From {
  attrs     :: SelectAttrs,
  tables    :: [Select],
  criteria  :: [HSql.SqlExpr],
  groupBy   :: Maybe (NEL.NonEmpty HSql.SqlExpr),
  orderBy   :: [(HSql.SqlExpr, HSql.SqlOrder)],
  limit     :: Maybe Int,
  offset    :: Maybe Int
  }
          deriving Show

data Join = Join {
  jJoinType   :: JoinType,
  jTables     :: (Select, Select),
  jCond       :: HSql.SqlExpr
  }
                deriving Show

data Values = Values {
  vAttrs  :: SelectAttrs,
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

sql :: ([HPQ.PrimExpr], PQ.PrimQuery, T.Tag) -> Select
sql (pes, pq, t) = SelectFrom $ newSelect { attrs = SelectAttrs (ensureColumns (makeAttrs pes))
                                          , tables = [pqSelect] }
  where pqSelect = PQ.foldPrimQuery sqlQueryGenerator pq
        makeAttrs = flip (zipWith makeAttr) [1..]
        makeAttr pe i = sqlBinding (Symbol ("result" ++ show (i :: Int)) t, pe)

unit :: Select
unit = SelectFrom newSelect { attrs  = SelectAttrs (ensureColumns []) }

baseTable :: String -> [(Symbol, HPQ.PrimExpr)] -> Select
baseTable name columns = SelectFrom $
    newSelect { attrs = SelectAttrs (ensureColumns (map sqlBinding columns))
              , tables = [Table (HSql.SqlTable name)] }

product :: NEL.NonEmpty Select -> [HPQ.PrimExpr] -> Select
product ss pes = SelectFrom $
    newSelect { tables = NEL.toList ss
              , criteria = map sqlExpr pes }

aggregate :: [(Symbol, (Maybe HPQ.AggrOp, HPQ.PrimExpr))] -> Select -> Select
aggregate aggrs s = SelectFrom $ newSelect { attrs = SelectAttrs
                                               (ensureColumns (map attr aggrs))
                                           , tables = [s]
                                           , groupBy = (Just . groupBy') aggrs }
  where --- Grouping by an empty list is not the identity function!
        --- In fact it forms one single group.  Syntactically one
        --- cannot group by nothing in SQL, so we just group by a
        --- constant instead.  Because "GROUP BY 0" means group by the
        --- zeroth column, we instead use an expression rather than a
        --- constant.
        handleEmpty :: [HSql.SqlExpr] -> NEL.NonEmpty HSql.SqlExpr
        handleEmpty =
          M.fromMaybe (return (HSql.FunSqlExpr "COALESCE" [HSql.ConstSqlExpr "0"]))
          . NEL.nonEmpty

        groupBy' :: [(symbol, (Maybe aggrOp, HPQ.PrimExpr))]
                 -> NEL.NonEmpty HSql.SqlExpr
        groupBy' = (handleEmpty
                    . map sqlExpr
                    . map expr
                    . filter (M.isNothing . aggrOp))
        attr = sqlBinding . Arr.second (uncurry aggrExpr)
        expr (_, (_, e)) = e
        aggrOp (_, (x, _)) = x


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

join :: PQ.JoinType -> HPQ.PrimExpr -> Select -> Select -> Select
join j cond s1 s2 = SelectJoin Join { jJoinType = joinType j
                                    , jTables = (s1, s2)
                                    , jCond = sqlExpr cond }

-- Postgres seems to name columns of VALUES clauses "column1",
-- "column2", ... . I'm not sure to what extent it is customisable or
-- how robust it is to rely on this
values :: [Symbol] -> [[HPQ.PrimExpr]] -> Select
values columns pes = SelectValues Values { vAttrs  = SelectAttrs (mkColumns columns)
                                         , vValues = (map . map) sqlExpr pes }
  where mkColumns = ensureColumns . zipWith (flip (curry (sqlBinding . Arr.second mkColumn))) [1..]
        mkColumn i = (HPQ.BaseTableAttrExpr . ("column" ++) . show) (i::Int)

binary :: PQ.BinOp -> [(Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))]
       -> (Select, Select) -> Select
binary op pes (select1, select2) = SelectBinary Binary {
  bOp = binOp op,
  bSelect1 = SelectFrom newSelect { attrs = SelectAttrs
                                      (ensureColumns (map (mkColumn fst) pes)),
                                    tables = [select1] },
  bSelect2 = SelectFrom newSelect { attrs = SelectAttrs
                                      (ensureColumns (map (mkColumn snd) pes)),
                                    tables = [select2] }
  }
  where mkColumn e = sqlBinding . Arr.second e

joinType :: PQ.JoinType -> JoinType
joinType PQ.LeftJoin = LeftJoin

binOp :: PQ.BinOp -> BinOp
binOp o = case o of
  PQ.Except   -> Except
  PQ.Union    -> Union
  PQ.UnionAll -> UnionAll

newSelect :: From
newSelect = From {
  attrs     = Star,
  tables    = [],
  criteria  = [],
  groupBy   = Nothing,
  orderBy   = [],
  limit     = Nothing,
  offset    = Nothing
  }

sqlExpr :: HPQ.PrimExpr -> HSql.SqlExpr
sqlExpr = SG.sqlExpr SD.defaultSqlGenerator

sqlBinding :: (Symbol, HPQ.PrimExpr) -> (HSql.SqlExpr, Maybe HSql.SqlColumn)
sqlBinding (Symbol sym t, pe) =
  (sqlExpr pe, Just (HSql.SqlColumn (T.tagWith t sym)))

ensureColumns :: [(HSql.SqlExpr, Maybe a)]
              -> NEL.NonEmpty (HSql.SqlExpr, Maybe a)
ensureColumns = M.fromMaybe (return (HSql.ConstSqlExpr "0", Nothing))
                . NEL.nonEmpty
