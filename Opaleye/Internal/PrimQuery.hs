module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Data.List.NonEmpty as NEL
import qualified Database.HaskellDB.PrimQuery as PQ

type Symbol = String

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show

data BinOp = Except | Union | UnionAll deriving Show
data JoinType = LeftJoin deriving Show

-- We use a 'NEL.NonEmpty' for Product because otherwise we'd have to check
-- for emptiness explicity in the SQL generation phase.
data PrimQuery = Unit
               | BaseTable String [(Symbol, PQ.PrimExpr)]
               | Product (NEL.NonEmpty PrimQuery) [PQ.PrimExpr]
               | Aggregate [(Symbol, Maybe PQ.AggrOp, PQ.PrimExpr)] PrimQuery
               | Order [PQ.OrderExpr] PrimQuery
               | Limit LimitOp PrimQuery
               | Join JoinType [(Symbol, PQ.PrimExpr)] PQ.PrimExpr PrimQuery PrimQuery
               | Values [Symbol] [[PQ.PrimExpr]]
               | Binary BinOp [(Symbol, (PQ.PrimExpr, PQ.PrimExpr))] (PrimQuery, PrimQuery)
                 deriving Show

type PrimQueryFold p = ( p
                       , String -> [(Symbol, PQ.PrimExpr)] -> p
                       , NEL.NonEmpty p -> [PQ.PrimExpr] -> p
                       , [(Symbol, Maybe PQ.AggrOp, PQ.PrimExpr)] -> p -> p
                       , [PQ.OrderExpr] -> p -> p
                       , LimitOp -> p -> p
                       , JoinType -> [(Symbol, PQ.PrimExpr)] -> PQ.PrimExpr -> p -> p -> p
                       , [Symbol] -> [[PQ.PrimExpr]] -> p
                       , BinOp -> [(Symbol, (PQ.PrimExpr, PQ.PrimExpr))] -> (p, p) -> p
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

restrict :: PQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (return primQ) [cond]

isUnit :: PrimQuery -> Bool
isUnit Unit = True
isUnit _    = False
