module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Database.HaskellDB.PrimQuery as PQ

type Symbol = String

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show

data BinOp = Difference | Union | UnionAll

data PrimQuery = BaseTable String [(Symbol, String)]
               | Product [PrimQuery] [PQ.PrimExpr]
               | Aggregate [(Symbol, Maybe PQ.AggrOp, PQ.PrimExpr)] PrimQuery
               | Order [PQ.OrderExpr] PrimQuery
               | Limit LimitOp PrimQuery
--               | Binary BinOp [(PQ.PrimExpr, PQ.PrimExpr)] (PrimQuery, PrimQuery)
                 deriving Show

type PrimQueryFold p = ( String -> [(Symbol, String)] -> p
                       , [p] -> [PQ.PrimExpr] -> p
                       , [(Symbol, Maybe PQ.AggrOp, PQ.PrimExpr)] -> p -> p
                       , [PQ.OrderExpr] -> p -> p
                       , LimitOp -> p -> p
--                       , BinOp -> [(PQ.PrimExpr, PQ.PrimExpr)] -> (p, p) -> p
                       )

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery (baseTable, product, aggregate, order, limit) = fold
  where fold primQ = case primQ of
          BaseTable n s              -> baseTable n s
          Product pqs pes            -> product (map fold pqs) pes
          Aggregate aggrs pq         -> aggregate aggrs (fold pq)
          Order pes pq               -> order pes (fold pq)
          Limit op pq                -> limit op (fold pq)
--          Binary binop pes (pq, pq') -> binary binop pes (fold pq, fold pq')
