module Opaleye.Internal.PrimQuery where

import           Prelude hiding (product)

import qualified Data.List.NonEmpty as NEL
import           Data.Semigroup (Semigroup, (<>))
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

data SemijoinType = Semi | Anti deriving Show

data TableIdentifier = TableIdentifier
  { tiSchemaName :: Maybe String
  , tiTableName  :: String
  } deriving Show

tiToSqlTable :: TableIdentifier -> HSql.SqlTable
tiToSqlTable ti = HSql.SqlTable { HSql.sqlTableSchemaName = tiSchemaName ti
                                , HSql.sqlTableName       = tiTableName ti }

type Bindings a = [(Symbol, a)]

data Lateral = NonLateral | Lateral
  deriving Show

instance Semigroup Lateral where
  NonLateral <> NonLateral = NonLateral
  _ <> _ = Lateral

instance Monoid Lateral where
  mappend = (<>)
  mempty = NonLateral

aLeftJoin :: HPQ.PrimExpr -> PrimQuery -> PrimQueryArr
aLeftJoin cond primQuery' = PrimQueryArr $ \lat primQueryL ->
  Join LeftJoin cond (NonLateral, primQueryL) (lat, primQuery')

-- The function 'Lateral -> PrimQuery -> PrimQuery' represents a
-- select arrow in the following way:
--
--    Lateral
-- -- ^ Whether to join me laterally
-- -> PrimQuery
-- -- ^ The query that I will be joined after.  If I refer to columns
-- -- in here in a way that is only valid when I am joined laterally,
-- -- then Lateral must be passed in as the argument above.
-- -> PrimQuery
-- -- ^ The result after joining me
--
-- It is *always* valid to pass Lateral as the first argument.  So why
-- wouldn't we do that?  Because we don't want to generate lateral
-- subqueries if they are not needed; it might have performance
-- implications.  Even though there is good evidence that it *doesn't*
-- have performance implications
-- (https://github.com/tomjaguarpaw/haskell-opaleye/pull/480) we still
-- want to be cautious.
newtype PrimQueryArr =
  PrimQueryArr { runPrimQueryArr :: Lateral -> PrimQuery -> PrimQuery }

instance Semigroup PrimQueryArr where
  PrimQueryArr f1 <> PrimQueryArr f2 = PrimQueryArr (\lat -> f2 lat . f1 lat)

instance Monoid PrimQueryArr where
  mappend = (<>)
  mempty = PrimQueryArr (\_ -> id)

lateral :: PrimQueryArr -> PrimQueryArr
lateral (PrimQueryArr pq) = PrimQueryArr (\_ -> pq Lateral)

toPrimQuery :: PrimQueryArr -> PrimQuery
toPrimQuery (PrimQueryArr f) = f NonLateral Unit

-- We use a 'NEL.NonEmpty' for Product because otherwise we'd have to check
-- for emptiness explicity in the SQL generation phase.

-- The type parameter 'a' is used to control whether the 'Empty'
-- constructor can appear.  If 'a' = '()' then it can appear.  If 'a'
-- = 'Void' then it cannot.  When we create queries it is more
-- convenient to allow 'Empty', but it is hard to represent 'Empty' in
-- SQL so we remove it in 'Optimize' and set 'a = Void'.
data PrimQuery' a = Unit
                  -- Remove the Empty constructor in 0.10
                  | Empty     a
                  | BaseTable TableIdentifier (Bindings HPQ.PrimExpr)
                  | Product   (NEL.NonEmpty (Lateral, PrimQuery' a)) [HPQ.PrimExpr]
                  -- | The subqueries to take the product of and the
                  --   restrictions to apply
                  | Aggregate (Bindings (Maybe (HPQ.AggrOp,
                                                [HPQ.OrderExpr],
                                                HPQ.AggrDistinct),
                                          HPQ.Symbol))
                              (PrimQuery' a)
                  -- | Represents both @DISTINCT ON@ and @ORDER BY@
                  --   clauses. In order to represent valid SQL only,
                  --   @DISTINCT ON@ expressions are always
                  --   interpreted as the first @ORDER BY@s when
                  --   present, preceding any in the provided list.
                  --   See 'Opaleye.Internal.Sql.distinctOnOrderBy'.
                  | DistinctOnOrderBy (Maybe (NEL.NonEmpty HPQ.PrimExpr))
                                      [HPQ.OrderExpr]
                                      (PrimQuery' a)
                  | Limit     LimitOp (PrimQuery' a)
                  | Join      JoinType
                              HPQ.PrimExpr
                              (Lateral, PrimQuery' a)
                              (Lateral, PrimQuery' a)
                  | Semijoin  SemijoinType (PrimQuery' a) (PrimQuery' a)
                  | Exists    Symbol (PrimQuery' a)
                  | Values    [Symbol] (NEL.NonEmpty [HPQ.PrimExpr])
                  | Binary    BinOp
                              (PrimQuery' a, PrimQuery' a)
                  | Label     String (PrimQuery' a)
                  | RelExpr   HPQ.PrimExpr (Bindings HPQ.PrimExpr)
                  | Rebind    Bool
                              (Bindings HPQ.PrimExpr)
                              (PrimQuery' a)
                  | ForUpdate (PrimQuery' a)
                  -- We may support more locking clauses than just
                  -- ForUpdate in the future
                  --
                  -- https://www.postgresql.org/docs/current/sql-select.html#SQL-FOR-UPDATE-SHARE
                 deriving Show

type PrimQuery = PrimQuery' ()
type PrimQueryFold = PrimQueryFold' ()

data PrimQueryFold' a p = PrimQueryFold
  { unit              :: p
  , empty             :: a -> p
  , baseTable         :: TableIdentifier -> Bindings HPQ.PrimExpr -> p
  , product           :: NEL.NonEmpty (Lateral, p) -> [HPQ.PrimExpr] -> p
  , aggregate         :: Bindings (Maybe
                             (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct),
                                   HPQ.Symbol)
                      -> p
                      -> p
  , distinctOnOrderBy :: Maybe (NEL.NonEmpty HPQ.PrimExpr)
                      -> [HPQ.OrderExpr]
                      -> p
                      -> p
  , limit             :: LimitOp -> p -> p
  , join              :: JoinType
                      -> HPQ.PrimExpr
                      -> (Lateral, p)
                      -> (Lateral, p)
                      -> p
  , semijoin          :: SemijoinType -> p -> p -> p
  , exists            :: Symbol -> p -> p
  , values            :: [Symbol] -> NEL.NonEmpty [HPQ.PrimExpr] -> p
  , binary            :: BinOp
                      -> (p, p)
                      -> p
  , label             :: String -> p -> p
  , relExpr           :: HPQ.PrimExpr -> Bindings HPQ.PrimExpr -> p
    -- ^ A relation-valued expression
  , rebind            :: Bool -> Bindings HPQ.PrimExpr -> p -> p
  , forUpdate         :: p -> p
  }


primQueryFoldDefault :: PrimQueryFold' a (PrimQuery' a)
primQueryFoldDefault = PrimQueryFold
  { unit              = Unit
  , empty             = Empty
  , baseTable         = BaseTable
  , product           = Product
  , aggregate         = Aggregate
  , distinctOnOrderBy = DistinctOnOrderBy
  , limit             = Limit
  , join              = Join
  , semijoin          = Semijoin
  , values            = Values
  , binary            = Binary
  , label             = Label
  , relExpr           = RelExpr
  , exists            = Exists
  , rebind            = Rebind
  , forUpdate         = ForUpdate
  }

foldPrimQuery :: PrimQueryFold' a p -> PrimQuery' a -> p
foldPrimQuery f = fix fold
  where fold self primQ = case primQ of
          Unit                        -> unit              f
          Empty a                     -> empty             f a
          BaseTable ti syms           -> baseTable         f ti syms
          Product qs pes              -> product           f (fmap (fmap self) qs) pes
          Aggregate aggrs q           -> aggregate         f aggrs (self q)
          DistinctOnOrderBy dxs oxs q -> distinctOnOrderBy f dxs oxs (self q)
          Limit op q                  -> limit             f op (self q)
          Join j cond q1 q2           -> join              f j cond (fmap self q1) (fmap self q2)
          Semijoin j q1 q2            -> semijoin          f j (self q1) (self q2)
          Values ss pes               -> values            f ss pes
          Binary binop (q1, q2)       -> binary            f binop (self q1, self q2)
          Label l pq                  -> label             f l (self pq)
          RelExpr pe syms             -> relExpr           f pe syms
          Exists s q                  -> exists            f s (self q)
          Rebind star pes q           -> rebind            f star pes (self q)
          ForUpdate q                 -> forUpdate         f (self q)
        fix g = let x = g x in x

times :: Lateral -> PrimQuery -> PrimQuery -> PrimQuery
times lat q q' = Product (pure q NEL.:| [(lat, q')]) []

restrict :: HPQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (return (pure primQ)) [cond]

isUnit :: PrimQuery' a -> Bool
isUnit Unit = True
isUnit _    = False
