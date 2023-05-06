{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
            | SelectSemijoin Semijoin
            | SelectValues Values
            | SelectBinary Binary
            | SelectLabel Label
            | SelectExists Exists
            | SelectWith With
            deriving Show

data SelectAttrs =
    Star
  | SelectAttrs (NEL.NonEmpty (HSql.SqlExpr, Maybe HSql.SqlColumn))
  | SelectAttrsStar (NEL.NonEmpty (HSql.SqlExpr, Maybe HSql.SqlColumn))
  deriving Show

data From = From {
  attrs      :: SelectAttrs,
  tables     :: [(Lateral, Select)],
  criteria   :: [HSql.SqlExpr],
  groupBy    :: Maybe (NEL.NonEmpty HSql.SqlExpr),
  orderBy    :: [(HSql.SqlExpr, HSql.SqlOrder)],
  distinctOn :: Maybe (NEL.NonEmpty HSql.SqlExpr),
  limit      :: Maybe Int,
  offset     :: Maybe Int,
  for        :: Maybe LockStrength
  }
          deriving Show

data Join = Join {
  jJoinType   :: JoinType,
  jTables     :: ((Lateral, Select), (Lateral, Select)),
  jCond       :: HSql.SqlExpr
  }
                deriving Show

data Semijoin = Semijoin
  { sjType     :: SemijoinType
  , sjTable    :: Select
  , sjCriteria :: Select
  } deriving Show

data Values = Values {
  vAttrs  :: SelectAttrs,
  vValues :: [[HSql.SqlExpr]]
} deriving Show

data Binary = Binary {
  bOp :: BinOp,
  bSelect1 :: Select,
  bSelect2 :: Select
} deriving Show

data JoinType = LeftJoin | RightJoin | FullJoin deriving Show
data SemijoinType = Semi | Anti deriving Show
data BinOp = Except | ExceptAll | Union | UnionAll | Intersect | IntersectAll deriving Show
data Lateral = Lateral | NonLateral deriving Show
data LockStrength = Update deriving Show
data Recursive = NonRecursive | Recursive deriving Show
data With = With {
  wTable     :: HSql.SqlTable, -- The name of the result, i.e. WITH <name> AS
  wCols      :: [HSql.SqlColumn],
  wRecursive :: Recursive,
  wWith      :: Select,
  wSelect    :: Select
} deriving Show


data Label = Label {
  lLabel  :: String,
  lSelect :: Select
} deriving Show

data Returning a = Returning a (NEL.NonEmpty HSql.SqlExpr)

data Exists = Exists
  { existsBinding :: Symbol
  , existsTable :: Select
  } deriving Show

sqlQueryGenerator :: PQ.PrimQueryFold' V.Void Select
sqlQueryGenerator = PQ.PrimQueryFold
  { PQ.unit              = unit
  , PQ.empty             = empty
  , PQ.baseTable         = baseTable
  , PQ.product           = product
  , PQ.aggregate         = aggregate
  , PQ.window            = window
  , PQ.distinctOnOrderBy = distinctOnOrderBy
  , PQ.limit             = limit_
  , PQ.join              = join
  , PQ.semijoin          = semijoin
  , PQ.values            = values
  , PQ.binary            = binary
  , PQ.label             = label
  , PQ.relExpr           = relExpr
  , PQ.exists            = exists
  , PQ.rebind            = rebind
  , PQ.forUpdate         = forUpdate
  , PQ.with              = with
  }

exists :: Symbol -> Select -> Select
exists binding table = SelectExists (Exists binding table)

sql :: ([HPQ.PrimExpr], PQ.PrimQuery' V.Void, T.Tag) -> Select
sql (pes, pq, t) = SelectFrom $ newSelect { attrs = SelectAttrs (ensureColumns (makeAttrs pes))
                                          , tables = oneTable pqSelect }
  where pqSelect = PQ.foldPrimQuery sqlQueryGenerator pq
        makeAttrs = flip (zipWith makeAttr) [1..]
        makeAttr pe i = sqlBinding (Symbol ("result" ++ show (i :: Int)) t, pe)

unit :: Select
unit = SelectFrom newSelect { attrs  = SelectAttrs (ensureColumns []) }

empty :: V.Void -> select
empty = V.absurd

oneTable :: t -> [(Lateral, t)]
oneTable t = [(NonLateral, t)]

baseTable :: PQ.TableIdentifier -> [(Symbol, HPQ.PrimExpr)] -> Select
baseTable ti columns = SelectFrom $
    newSelect { attrs = SelectAttrs (ensureColumns (map sqlBinding columns))
              , tables = oneTable (Table (HSql.SqlTable (PQ.tiSchemaName ti) (PQ.tiTableName ti))) }

product :: NEL.NonEmpty (PQ.Lateral, Select) -> [HPQ.PrimExpr] -> Select
product ss pes = SelectFrom $
    newSelect { tables = NEL.toList ss'
              , criteria = map sqlExpr pes }
  where ss' = flip fmap ss $ Arr.first $ \case
          PQ.Lateral    -> Lateral
          PQ.NonLateral -> NonLateral

aggregate :: PQ.Bindings (HPQ.Aggr, HPQ.Symbol)
          -> Select
          -> Select
aggregate aggrs' s =
  SelectFrom $ newSelect { attrs = SelectAttrs (ensureColumns (map attr aggrs))
                         , tables = oneTable s
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
        handleEmpty = ensureColumnsGen SP.deliteral

        aggrs = (map . Arr.second . Arr.second) HPQ.AttrExpr aggrs'

        groupBy' :: [(symbol, (HPQ.Aggr, HPQ.PrimExpr))]
                 -> NEL.NonEmpty HSql.SqlExpr
        groupBy' = handleEmpty
                   . map sqlExpr
                   . map expr
                   . filter (M.isNothing . aggrOp)
        attr = sqlBinding . Arr.second (uncurry aggrExpr)
        expr (_, (_, e)) = e
        aggrOp (_, (x, _)) = x

aggrExpr :: HPQ.Aggr -> HPQ.PrimExpr -> HPQ.PrimExpr
aggrExpr = \case
  Nothing -> id
  Just (op, ord, distinct) -> \e -> HPQ.AggrExpr distinct op e ord

window :: PQ.Bindings (HPQ.WndwOp, HPQ.Partition) -> Select -> Select
window wndws' s = SelectFrom $ newSelect
  { attrs = SelectAttrsStar (ensureColumns (map (sqlBinding . fmap (uncurry HPQ.WndwExpr)) wndws'))
  , tables = oneTable s
  }

distinctOnOrderBy :: Maybe (NEL.NonEmpty HPQ.PrimExpr) -> [HPQ.OrderExpr] -> Select -> Select
distinctOnOrderBy distinctExprs orderExprs s = SelectFrom $ newSelect
    { tables     = oneTable s
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
limit_ lo s = SelectFrom $ newSelect { tables = oneTable s
                                     , limit = limit'
                                     , offset = offset' }
  where (limit', offset') = case lo of
          PQ.LimitOp n         -> (Just n, Nothing)
          PQ.OffsetOp n        -> (Nothing, Just n)
          PQ.LimitOffsetOp l o -> (Just l, Just o)

join :: PQ.JoinType
     -> HPQ.PrimExpr
     -> (PQ.Lateral, Select)
     -> (PQ.Lateral, Select)
     -> Select
join j cond s1 s2 =
  SelectJoin Join { jJoinType = joinType j
                  , jTables   = (Arr.first lat s1, Arr.first lat s2)
                  , jCond     = sqlExpr cond }
  where lat = \case
          PQ.Lateral -> Lateral
          PQ.NonLateral -> NonLateral

semijoin :: PQ.SemijoinType -> Select -> Select -> Select
semijoin t q1 q2 = SelectSemijoin (Semijoin (semijoinType t) q1 q2)


-- Postgres seems to name columns of VALUES clauses "column1",
-- "column2", ... . I'm not sure to what extent it is customisable or
-- how robust it is to rely on this
values :: [Symbol] -> NEL.NonEmpty [HPQ.PrimExpr] -> Select
values columns pes = SelectValues Values { vAttrs  = SelectAttrs (mkColumns columns)
                                         , vValues = NEL.toList ((fmap . map) sqlExpr pes) }
  where mkColumns = ensureColumns . zipWith (flip (curry (sqlBinding . Arr.second mkColumn))) [1..]
        mkColumn i = (HPQ.BaseTableAttrExpr . ("column" ++) . show) (i::Int)

binary :: PQ.BinOp -> (Select, Select) -> Select
binary op (select1, select2) = SelectBinary Binary {
  bOp = binOp op,
  bSelect1 = select1,
  bSelect2 = select2
  }

with :: PQ.Recursive -> Symbol -> [Symbol]-> Select -> Select -> Select
with recursive name cols wWith wSelect = SelectWith $ With {..}
  where
   wTable = HSql.SqlTable Nothing (sqlSymbol name)
   wRecursive = case recursive of
     PQ.NonRecursive -> NonRecursive
     PQ.Recursive -> Recursive
   wCols = map (HSql.SqlColumn . sqlSymbol) cols

joinType :: PQ.JoinType -> JoinType
joinType PQ.LeftJoin = LeftJoin
joinType PQ.RightJoin = RightJoin
joinType PQ.FullJoin = FullJoin

semijoinType :: PQ.SemijoinType -> SemijoinType
semijoinType PQ.Semi = Semi
semijoinType PQ.Anti = Anti

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
  offset     = Nothing,
  for        = Nothing
  }

sqlExpr :: HPQ.PrimExpr -> HSql.SqlExpr
sqlExpr = SG.sqlExpr SD.defaultSqlGenerator

sqlSymbol :: Symbol -> String
sqlSymbol (Symbol sym t) = T.tagWith t sym

sqlBinding :: (Symbol, HPQ.PrimExpr) -> (HSql.SqlExpr, Maybe HSql.SqlColumn)
sqlBinding (s, pe) = (sqlExpr pe, Just (HSql.SqlColumn (sqlSymbol s)))

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
              , tables = oneTable (RelExpr (sqlExpr pe))
              }

rebind :: Bool -> [(Symbol, HPQ.PrimExpr)] -> Select -> Select
rebind star pes select = SelectFrom newSelect
  { attrs = selectAttrs (ensureColumns (map sqlBinding pes))
  , tables = oneTable select
  }
  where selectAttrs = case star of
          True  -> SelectAttrsStar
          False -> SelectAttrs

forUpdate :: Select -> Select
forUpdate s = SelectFrom newSelect {
    tables = [(NonLateral, s)]
  , for = Just Update
  }
