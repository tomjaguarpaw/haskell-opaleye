module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Opaleye.Internal.NEList as NE

type Symbol = String

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show

data BinOp = Difference | Union | UnionAll

-- We use an NEList for Product because otherwise we'd have to check
-- for emptiness explicity in the SQL generation phase.
data PrimQuery = Unit
               | BaseTable String [(Symbol, String)]
               | Product (NE.NEList PrimQuery) [PQ.PrimExpr]
               | Aggregate [(Symbol, Maybe PQ.AggrOp, PQ.PrimExpr)] PrimQuery
               | Order [PQ.OrderExpr] PrimQuery
               | Limit LimitOp PrimQuery
               | LeftJoin [(Symbol, PQ.PrimExpr)] PQ.PrimExpr PrimQuery PrimQuery
--               | Binary BinOp [(PQ.PrimExpr, PQ.PrimExpr)] (PrimQuery, PrimQuery)
                 deriving Show

type PrimQueryFold p = ( p
                       , String -> [(Symbol, String)] -> p
                       , NE.NEList p -> [PQ.PrimExpr] -> p
                       , [(Symbol, Maybe PQ.AggrOp, PQ.PrimExpr)] -> p -> p
                       , [PQ.OrderExpr] -> p -> p
                       , LimitOp -> p -> p
                       , [(Symbol, PQ.PrimExpr)] -> PQ.PrimExpr -> p -> p -> p
--                       , BinOp -> [(PQ.PrimExpr, PQ.PrimExpr)] -> (p, p) -> p
                       )

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery (unit, baseTable, product, aggregate, order, limit, leftJoin)
  = fold where fold primQ = case primQ of
                 Unit                       -> unit
                 BaseTable n s              -> baseTable n s
                 Product pqs pes            -> product (fmap fold pqs) pes
                 Aggregate aggrs pq         -> aggregate aggrs (fold pq)
                 Order pes pq               -> order pes (fold pq)
                 Limit op pq                -> limit op (fold pq)
                 LeftJoin pes cond q1 q2    -> leftJoin pes cond (fold q1) (fold q2)
--          Binary binop pes (pq, pq') -> binary binop pes (fold pq, fold pq')

times :: PrimQuery -> PrimQuery -> PrimQuery
times q q' = Product (NE.NEList q [q']) []

restrict :: PQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (NE.singleton primQ) [cond]

isUnit :: PrimQuery -> Bool
isUnit Unit = True
isUnit _    = False
