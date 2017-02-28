module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Data.List.NonEmpty as NEL
import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye.Internal.HaskellDB.PrimQuery (Symbol)

data LimitOp = LimitOp Int | OffsetOp Int | LimitOffsetOp Int Int
             deriving Show

data BinOp = Except
           | ExceptAll
           | Union
           | UnionAll
           | Intersect
           | IntersectAll
             deriving Show

data JoinType = LeftJoin | RightJoin | FullJoin deriving Show

data TableIdentifier = TableIdentifier
  { tiSchemaName :: Maybe String
  , tiTableName  :: String
  } deriving Show

tiToSqlTable :: TableIdentifier -> HSql.SqlTable
tiToSqlTable ti = HSql.SqlTable { HSql.sqlTableSchemaName = tiSchemaName ti
                                , HSql.sqlTableName       = tiTableName ti }

type Bindings a = [(Symbol, a)]

-- We use a 'NEL.NonEmpty' for Product because otherwise we'd have to check
-- for emptiness explicity in the SQL generation phase.

-- The type parameter 'a' is used to control whether the 'Empty'
-- constructor can appear.  If 'a' = '()' then it can appear.  If 'a'
-- = 'Void' then it cannot.  When we create queries it is more
-- convenient to allow 'Empty', but it is hard to represent 'Empty' in
-- SQL so we remove it in 'Optimize' and set 'a = Void'.
data PrimQuery' a = Unit
                  | Empty     a
                  | BaseTable TableIdentifier (Bindings HPQ.PrimExpr)
                  | Product   (NEL.NonEmpty (PrimQuery' a)) [HPQ.PrimExpr]
                  | Aggregate (Bindings (Maybe (HPQ.AggrOp, [HPQ.OrderExpr]), HPQ.PrimExpr))
                              (PrimQuery' a)
                  | Order     [HPQ.OrderExpr] (PrimQuery' a)
                  | Limit     LimitOp (PrimQuery' a)
                  | Join      JoinType
                              HPQ.PrimExpr
                              (Bindings HPQ.PrimExpr)
                              (Bindings HPQ.PrimExpr)
                              (PrimQuery' a)
                              (PrimQuery' a)
                  | Antijoin  (PrimQuery' a) (PrimQuery' a)
                  | Values    [Symbol] (NEL.NonEmpty [HPQ.PrimExpr])
                  | Binary    BinOp
                              (Bindings (HPQ.PrimExpr, HPQ.PrimExpr))
                              (PrimQuery' a, PrimQuery' a)
                  | Label     String (PrimQuery' a)
                  | RelExpr   HPQ.PrimExpr (Bindings HPQ.PrimExpr)
                 deriving Show

type PrimQuery = PrimQuery' ()
type PrimQueryFold = PrimQueryFold' ()

data PrimQueryFold' a p = PrimQueryFold
  { unit      :: p
  , empty     :: a -> p
  , baseTable :: TableIdentifier -> (Bindings HPQ.PrimExpr) -> p
  , product   :: NEL.NonEmpty p -> [HPQ.PrimExpr] -> p
  , aggregate :: (Bindings (Maybe (HPQ.AggrOp, [HPQ.OrderExpr]), HPQ.PrimExpr)) -> p -> p
  , order     :: [HPQ.OrderExpr] -> p -> p
  , limit     :: LimitOp -> p -> p
  , join      :: JoinType
              -> HPQ.PrimExpr
              -> (Bindings HPQ.PrimExpr)
              -> (Bindings HPQ.PrimExpr)
              -> p
              -> p
              -> p
  , antijoin  :: p -> p -> p
  , values    :: [Symbol] -> (NEL.NonEmpty [HPQ.PrimExpr]) -> p
  , binary    :: BinOp -> (Bindings (HPQ.PrimExpr, HPQ.PrimExpr)) -> (p, p) -> p
  , label     :: String -> p -> p
  , relExpr   :: HPQ.PrimExpr -> (Bindings HPQ.PrimExpr) -> p
    -- ^ A relation-valued expression
  }


primQueryFoldDefault :: PrimQueryFold' a (PrimQuery' a)
primQueryFoldDefault = PrimQueryFold
  { unit      = Unit
  , empty     = Empty
  , baseTable = BaseTable
  , product   = Product
  , aggregate = Aggregate
  , order     = Order
  , limit     = Limit
  , join      = Join
  , values    = Values
  , binary    = Binary
  , label     = Label
  , relExpr   = RelExpr
  , antijoin  = Antijoin
  }

foldPrimQuery :: PrimQueryFold' a p -> PrimQuery' a -> p
foldPrimQuery f = fix fold
  where fold self primQ = case primQ of
          Unit                      -> unit      f
          Empty a                   -> empty     f a
          BaseTable ti syms         -> baseTable f ti syms
          Product qs pes            -> product   f (fmap self qs) pes
          Aggregate aggrs q         -> aggregate f aggrs (self q)
          Order pes q               -> order     f pes (self q)
          Limit op q                -> limit     f op (self q)
          Join j cond pe1 pe2 q1 q2 -> join      f j cond pe1 pe2 (self q1) (self q2)
          Values ss pes             -> values    f ss pes
          Binary binop pes (q1, q2) -> binary    f binop pes (self q1, self q2)
          Label l pq                -> label     f l (self pq)
          RelExpr pe syms           -> relExpr   f pe syms
          Antijoin q1 q2            -> antijoin  f (self q1) (self q2)
        fix g = let x = g x in x

times :: PrimQuery -> PrimQuery -> PrimQuery
times q q' = Product (q NEL.:| [q']) []

restrict :: HPQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (return primQ) [cond]

isUnit :: PrimQuery' a -> Bool
isUnit Unit = True
isUnit _    = False
