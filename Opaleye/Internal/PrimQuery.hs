module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Data.List.NonEmpty as NEL
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- I really want to make this abstract, i.e. `newtype Symbol = Symbol
-- String`, and then later improve it so that carries around a unique
-- ID, i.e. `data Symbol = Symbol String Int`.  This will make our SQL
-- generation a bit neater.  However we currently conflate column
-- names in base tables with our own internal column names and use
-- PrimQuery(AttrExpr) for both.
type Symbol = String

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show

data BinOp = Except | Union | UnionAll deriving Show
data JoinType = LeftJoin deriving Show

-- We use a 'NEL.NonEmpty' for Product because otherwise we'd have to check
-- for emptiness explicity in the SQL generation phase.
data PrimQuery = Unit
               | BaseTable String [(Symbol, HPQ.PrimExpr)]
               | Product (NEL.NonEmpty PrimQuery) [HPQ.PrimExpr]
               | Aggregate [(Symbol, (Maybe HPQ.AggrOp, HPQ.PrimExpr))] PrimQuery
               | Order [HPQ.OrderExpr] PrimQuery
               | Limit LimitOp PrimQuery
               | Join JoinType [(Symbol, HPQ.PrimExpr)] HPQ.PrimExpr PrimQuery PrimQuery
               | Values [Symbol] [[HPQ.PrimExpr]]
               | Binary BinOp [(Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))] (PrimQuery, PrimQuery)
                 deriving Show

type PrimQueryFold p = ( p
                       , String -> [(Symbol, HPQ.PrimExpr)] -> p
                       , NEL.NonEmpty p -> [HPQ.PrimExpr] -> p
                       , [(Symbol, (Maybe HPQ.AggrOp, HPQ.PrimExpr))] -> p -> p
                       , [HPQ.OrderExpr] -> p -> p
                       , LimitOp -> p -> p
                       , JoinType -> [(Symbol, HPQ.PrimExpr)] -> HPQ.PrimExpr -> p -> p -> p
                       , [Symbol] -> [[HPQ.PrimExpr]] -> p
                       , BinOp -> [(Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))] -> (p, p) -> p
                       )

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery (unit, baseTable, product, aggregate, order, limit, join, values,
               binary)
  = fold where fold primQ = case primQ of
                 Unit                       -> unit
                 BaseTable n s              -> baseTable n s
                 Product pqs pes            -> product (fmap fold pqs) pes
                 Aggregate aggrs pq         -> aggregate aggrs (fold pq)
                 Order pes pq               -> order pes (fold pq)
                 Limit op pq                -> limit op (fold pq)
                 Join j pes cond q1 q2      -> join j pes cond (fold q1) (fold q2)
                 Values ss pes              -> values ss pes
                 Binary binop pes (pq, pq') -> binary binop pes (fold pq, fold pq')

times :: PrimQuery -> PrimQuery -> PrimQuery
times q q' = Product (q NEL.:| [q']) []

restrict :: HPQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (return primQ) [cond]

isUnit :: PrimQuery -> Bool
isUnit Unit = True
isUnit _    = False
