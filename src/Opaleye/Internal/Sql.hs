module Opaleye.Internal.Sql where

import           Prelude hiding (product)

import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye.Internal.HaskellDB.PrimQuery (Symbol(Symbol))
import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Default as SD
import qualified Opaleye.Internal.HaskellDB.Sql.Print as SP
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as SG
import qualified Opaleye.Internal.Tag as T

import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as M
import qualified Data.Void as V

import qualified Control.Arrow as Arr

data Select = SelectFrom From
            | Table HSql.SqlTable
            | RelExpr HSql.SqlExpr
            -- ^ A relation-valued expression
            | SelectJoin Join
            | SelectValues Values
            | SelectBinary Binary
            | SelectLabel Label
            | SelectExists Exists
            deriving Show

data SelectAttrs =
    Star
  | SelectAttrs (NEL.NonEmpty (HSql.SqlExpr, Maybe HSql.SqlColumn))
  | SelectAttrsStar (NEL.NonEmpty (HSql.SqlExpr, Maybe HSql.SqlColumn))
  deriving Show

data From = From {
  attrs      :: SelectAttrs,
  tables     :: [Select],
  criteria   :: [HSql.SqlExpr],
  groupBy    :: Maybe (NEL.NonEmpty HSql.SqlExpr),
  orderBy    :: [(HSql.SqlExpr, HSql.SqlOrder)],
  distinctOn :: Maybe (NEL.NonEmpty HSql.SqlExpr),
  limit      :: Maybe Int,
  offset     :: Maybe Int
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

data JoinType = LeftJoin | RightJoin | FullJoin | InnerJoinLateral | LeftJoinLateral deriving Show
data BinOp = Except | ExceptAll | Union | UnionAll | Intersect | IntersectAll deriving Show

data Label = Label {
  lLabel  :: String,
  lSelect :: Select
} deriving Show

data Returning a = Returning a (NEL.NonEmpty HSql.SqlExpr)

data Exists = Exists
  { existsBool :: Bool
  , existsTable :: Select
  , existsCriteria :: Select
  } deriving Show

sqlQueryGenerator :: PQ.PrimQueryFold' V.Void Select
sqlQueryGenerator = PQ.PrimQueryFold
  { PQ.unit              = unit
  , PQ.empty             = empty
  , PQ.baseTable         = baseTable
  , PQ.product           = product
  , PQ.aggregate         = aggregate
  , PQ.distinctOnOrderBy = distinctOnOrderBy
  , PQ.limit             = limit_
  , PQ.join              = join
  , PQ.values            = values
  , PQ.binary            = binary
  , PQ.label             = label
  , PQ.relExpr           = relExpr
  , PQ.existsf           = exists
  }

exists :: Bool -> Select -> Select -> Select
exists b q1 q2 = SelectExists (Exists b q1 q2)

sql :: ([HPQ.PrimExpr], PQ.PrimQuery' V.Void, T.Tag) -> Select
sql (pes, pq, t) = SelectFrom $ newSelect { attrs = SelectAttrs (ensureColumns (makeAttrs pes))
                                          , tables = [pqSelect] }
  where pqSelect = PQ.foldPrimQuery sqlQueryGenerator pq
        makeAttrs = flip (zipWith makeAttr) [1..]
        makeAttr pe i = sqlBinding (Symbol ("result" ++ show (i :: Int)) t, pe)

unit :: Select
unit = SelectFrom newSelect { attrs  = SelectAttrs (ensureColumns []) }

empty :: V.Void -> select
empty = V.absurd

baseTable :: PQ.TableIdentifier -> [(Symbol, HPQ.PrimExpr)] -> Select
baseTable ti columns = SelectFrom $
    newSelect { attrs = SelectAttrs (ensureColumns (map sqlBinding columns))
              , tables = [Table (HSql.SqlTable (PQ.tiSchemaName ti) (PQ.tiTableName ti))] }

product :: NEL.NonEmpty Select -> [HPQ.PrimExpr] -> Select
product ss pes = SelectFrom $
    newSelect { tables = NEL.toList ss
              , criteria = map sqlExpr pes }

aggregate :: [(Symbol,
               (Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct),
                HPQ.PrimExpr))]
          -> Select
          -> Select
aggregate aggrs s =
  SelectFrom $ newSelect { attrs = SelectAttrs (ensureColumns (map attr aggrs))
                         , tables = [s]
                         , groupBy = (Just . groupBy') aggrs }
  where --- Although in the presence of an aggregation function,
        --- grouping by an empty list is equivalent to omitting group
        --- by, the equivalence does not hold in the absence of an
        --- aggregation function.  In the absence of an aggregation
        --- function, group by of an empty list will return a single
        --- row (if there are any and zero rows otherwise).  A query
        --- without group by will return all rows.  This is a weakness
        --- of SQL.  Really there ought to be a separate SELECT
        --- AGGREGATE operation.
        ---
        --- Syntactically one cannot group by an empty list in SQL.
        --- We take the conservative approach of explicitly grouping
        --- by a constant if we are provided with an empty list of
        --- group bys.  This yields a single group.  (Alternatively,
        --- we could check whether any aggregation functions have been
        --- applied and only group by a constant in the case where
        --- none have.  That would make the generated SQL less noisy.)
        ---
        --- "GROUP BY 0" means group by the zeroth column so we
        --- instead use an expression rather than a constant.
        handleEmpty :: [HSql.SqlExpr] -> NEL.NonEmpty HSql.SqlExpr
        handleEmpty =
          M.fromMaybe (return (SP.deliteral (HSql.ConstSqlExpr "0")))
          . NEL.nonEmpty

        groupBy' :: [(symbol, (Maybe aggrOp, HPQ.PrimExpr))]
                 -> NEL.NonEmpty HSql.SqlExpr
        groupBy' = handleEmpty
                   . map sqlExpr
                   . map expr
                   . filter (M.isNothing . aggrOp)
        attr = sqlBinding . Arr.second (uncurry aggrExpr)
        expr (_, (_, e)) = e
        aggrOp (_, (x, _)) = x


aggrExpr :: Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct) -> HPQ.PrimExpr -> HPQ.PrimExpr
aggrExpr = maybe id (\(op, ord, distinct) e -> HPQ.AggrExpr distinct op e ord)

distinctOnOrderBy :: Maybe (NEL.NonEmpty HPQ.PrimExpr) -> [HPQ.OrderExpr] -> Select -> Select
distinctOnOrderBy distinctExprs orderExprs s = SelectFrom $ newSelect
    { tables     = [s]
    , distinctOn = fmap (SG.sqlExpr SD.defaultSqlGenerator) <$> distinctExprs
    , orderBy    = map (SD.toSqlOrder SD.defaultSqlGenerator) $
        -- Postgres requires all 'DISTINCT ON' expressions to appear before any other
        -- 'ORDER BY' expressions if there are any.
        maybe [] (map (HPQ.OrderExpr ascOp) . NEL.toList) distinctExprs ++ orderExprs
    }
    where
        ascOp = HPQ.OrderOp
            { HPQ.orderDirection = HPQ.OpAsc
            , HPQ.orderNulls     = HPQ.NullsLast }

limit_ :: PQ.LimitOp -> Select -> Select
limit_ lo s = SelectFrom $ newSelect { tables = [s]
                                     , limit = limit'
                                     , offset = offset' }
  where (limit', offset') = case lo of
          PQ.LimitOp n         -> (Just n, Nothing)
          PQ.OffsetOp n        -> (Nothing, Just n)
          PQ.LimitOffsetOp l o -> (Just l, Just o)

join :: PQ.JoinType
     -> HPQ.PrimExpr
     -> PQ.Bindings HPQ.PrimExpr
     -> PQ.Bindings HPQ.PrimExpr
     -> Select
     -> Select
     -> Select
join j cond pes1 pes2 s1 s2 =
  SelectJoin Join { jJoinType = joinType j
                  , jTables   = (selectFrom pes1 s1, selectFrom pes2 s2)
                  , jCond     = sqlExpr cond }
  where selectFrom pes s = SelectFrom $ newSelect {
            attrs  = SelectAttrsStar (ensureColumns (map sqlBinding pes))
          , tables = [s]
          }

-- Postgres seems to name columns of VALUES clauses "column1",
-- "column2", ... . I'm not sure to what extent it is customisable or
-- how robust it is to rely on this
values :: [Symbol] -> NEL.NonEmpty [HPQ.PrimExpr] -> Select
values columns pes = SelectValues Values { vAttrs  = SelectAttrs (mkColumns columns)
                                         , vValues = NEL.toList ((fmap . map) sqlExpr pes) }
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
joinType PQ.RightJoin = RightJoin
joinType PQ.FullJoin = FullJoin
joinType PQ.InnerJoinLateral = InnerJoinLateral
joinType PQ.LeftJoinLateral = LeftJoinLateral

binOp :: PQ.BinOp -> BinOp
binOp o = case o of
  PQ.Except       -> Except
  PQ.ExceptAll    -> ExceptAll
  PQ.Union        -> Union
  PQ.UnionAll     -> UnionAll
  PQ.Intersect    -> Intersect
  PQ.IntersectAll -> IntersectAll

newSelect :: From
newSelect = From {
  attrs      = Star,
  tables     = [],
  criteria   = [],
  groupBy    = Nothing,
  orderBy    = [],
  distinctOn = Nothing,
  limit      = Nothing,
  offset     = Nothing
  }

sqlExpr :: HPQ.PrimExpr -> HSql.SqlExpr
sqlExpr = SG.sqlExpr SD.defaultSqlGenerator

sqlBinding :: (Symbol, HPQ.PrimExpr) -> (HSql.SqlExpr, Maybe HSql.SqlColumn)
sqlBinding (Symbol sym t, pe) =
  (sqlExpr pe, Just (HSql.SqlColumn (T.tagWith t sym)))

ensureColumns :: [(HSql.SqlExpr, Maybe a)]
             -> NEL.NonEmpty (HSql.SqlExpr, Maybe a)
ensureColumns = ensureColumnsGen (\x -> (x,Nothing))

-- | For ensuring that we have at least one column in a SELECT or RETURNING
ensureColumnsGen :: (HSql.SqlExpr -> a)
              -> [a]
              -> NEL.NonEmpty a
ensureColumnsGen f = M.fromMaybe (return . f $ HSql.ConstSqlExpr "0")
                   . NEL.nonEmpty

label :: String -> Select -> Select
label l s = SelectLabel (Label l s)

-- Very similar to 'baseTable'
relExpr :: HPQ.PrimExpr -> [(Symbol, HPQ.PrimExpr)] -> Select
relExpr pe columns = SelectFrom $
    newSelect { attrs = SelectAttrs (ensureColumns (map sqlBinding columns))
              , tables = [RelExpr (sqlExpr pe)]
              }
