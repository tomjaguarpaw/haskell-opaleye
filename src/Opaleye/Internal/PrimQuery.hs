module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Data.List.NonEmpty as NEL
import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye.Internal.HaskellDB.PrimQuery (Symbol)

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show

data BinOp = Except | Union | UnionAll deriving Show
data JoinType = LeftJoin deriving Show

data TableIdentifier = TableIdentifier
  { tiSchemaName :: Maybe String
  , tiTableName  :: String
  } deriving Show

tiToSqlTable :: TableIdentifier -> HSql.SqlTable
tiToSqlTable ti = HSql.SqlTable { HSql.sqlTableSchemaName = tiSchemaName ti
                                , HSql.sqlTableName = tiTableName ti }


-- In the future it may make sense to introduce this datatype
-- type Bindings a = [(Symbol, a)]

-- We use a 'NEL.NonEmpty' for Product because otherwise we'd have to check
-- for emptiness explicity in the SQL generation phase.
data PrimQuery = Unit
               | BaseTable TableIdentifier [(Symbol, HPQ.PrimExpr)]
               | Product (NEL.NonEmpty PrimQuery) [HPQ.PrimExpr]
               | Aggregate [(Symbol, (Maybe HPQ.AggrOp, HPQ.PrimExpr))] PrimQuery
               | Order [HPQ.OrderExpr] PrimQuery
               | Limit LimitOp PrimQuery
               | Join JoinType HPQ.PrimExpr PrimQuery PrimQuery
               | Values [Symbol] [[HPQ.PrimExpr]]
               | Binary BinOp [(Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))] (PrimQuery, PrimQuery)
                 deriving Show

data PrimQueryFold p = PrimQueryFold
  { unit      :: p
  , baseTable :: TableIdentifier -> [(Symbol, HPQ.PrimExpr)] -> p
  , product   :: NEL.NonEmpty p -> [HPQ.PrimExpr] -> p
  , aggregate :: [(Symbol, (Maybe HPQ.AggrOp, HPQ.PrimExpr))] -> p -> p
  , order     :: [HPQ.OrderExpr] -> p -> p
  , limit     :: LimitOp -> p -> p
  , join      :: JoinType -> HPQ.PrimExpr -> p -> p -> p
  , values    :: [Symbol] -> [[HPQ.PrimExpr]] -> p
  , binary    :: BinOp -> [(Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))] -> (p, p) -> p
  }


primQueryFoldDefault :: PrimQueryFold PrimQuery
primQueryFoldDefault = PrimQueryFold
  { unit      = Unit
  , baseTable = BaseTable
  , product   = Product
  , aggregate = Aggregate
  , order     = Order
  , limit     = Limit
  , join      = Join
  , values    = Values
  , binary    = Binary }

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery f = fix fold
  where fold self primQ = case primQ of
          Unit                       -> unit      f
          BaseTable ti syms          -> baseTable f ti syms
          Product pqs pes            -> product   f (fmap self pqs) pes
          Aggregate aggrs pq         -> aggregate f aggrs (self pq)
          Order pes pq               -> order     f  pes (self pq)
          Limit op pq                -> limit     f op (self pq)
          Join j cond q1 q2          -> join      f j cond (self q1) (self q2)
          Values ss pes              -> values    f ss pes
          Binary binop pes (pq, pq') -> binary    f binop pes (self pq, self pq')
        fix f = let x = f x in x

times :: PrimQuery -> PrimQuery -> PrimQuery
times q q' = Product (q NEL.:| [q']) []

restrict :: HPQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (return primQ) [cond]

isUnit :: PrimQuery -> Bool
isUnit Unit = True
isUnit _    = False
