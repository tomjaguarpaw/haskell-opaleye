module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Data.List.NonEmpty as NEL
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye.Internal.HaskellDB.PrimQuery (Symbol)

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show

data BinOp = Except | Union | UnionAll deriving Show
data JoinType = LeftJoin deriving Show

-- In the future it may make sense to introduce this datatype
-- type Bindings a = [(Symbol, a)]

-- We use a 'NEL.NonEmpty' for Product because otherwise we'd have to check
-- for emptiness explicity in the SQL generation phase.
data PrimQuery = Unit
               | BaseTable String [(Symbol, HPQ.PrimExpr)]
               | Product (NEL.NonEmpty PrimQuery) [HPQ.PrimExpr]
               | Aggregate [(Symbol, (Maybe HPQ.AggrOp, HPQ.PrimExpr))] PrimQuery
               | Order [HPQ.OrderExpr] PrimQuery
               | Limit LimitOp PrimQuery
               | Join JoinType HPQ.PrimExpr PrimQuery PrimQuery
               | Values [Symbol] [[HPQ.PrimExpr]]
               | Binary BinOp [(Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))] (PrimQuery, PrimQuery)
                 deriving Show

type PrimQueryFold p = ( p
                       , String -> [(Symbol, HPQ.PrimExpr)] -> p
                       , NEL.NonEmpty p -> [HPQ.PrimExpr] -> p
                       , [(Symbol, (Maybe HPQ.AggrOp, HPQ.PrimExpr))] -> p -> p
                       , [HPQ.OrderExpr] -> p -> p
                       , LimitOp -> p -> p
                       , JoinType -> HPQ.PrimExpr -> p -> p -> p
                       , [Symbol] -> [[HPQ.PrimExpr]] -> p
                       , BinOp -> [(Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))] -> (p, p) -> p
                       )

foldPrimQuery :: PrimQueryFold p -> PrimQuery -> p
foldPrimQuery (unit, baseTable, product, aggregate, order, limit, join, values,
               binary) = fix fold
  where fold self primQ = case primQ of
          Unit                       -> unit
          BaseTable n s              -> baseTable n s
          Product pqs pes            -> product (fmap self pqs) pes
          Aggregate aggrs pq         -> aggregate aggrs (self pq)
          Order pes pq               -> order pes (self pq)
          Limit op pq                -> limit op (self pq)
          Join j cond q1 q2          -> join j cond (self q1) (self q2)
          Values ss pes              -> values ss pes
          Binary binop pes (pq, pq') -> binary binop pes (self pq, self pq')
        fix f = let x = f x in x

times :: PrimQuery -> PrimQuery -> PrimQuery
times q q' = Product (q NEL.:| [q']) []

restrict :: HPQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (return primQ) [cond]

isUnit :: PrimQuery -> Bool
isUnit Unit = True
isUnit _    = False
