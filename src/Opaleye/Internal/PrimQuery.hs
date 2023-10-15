{-# LANGUAGE LambdaCase #-}

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

data Recursive = NonRecursive | Recursive
  deriving Show

data Materialized = Materialized | NotMaterialized
  deriving Show

aLeftJoin :: HPQ.PrimExpr -> PrimQuery -> PrimQueryArr
aLeftJoin cond primQuery' = PrimQueryArr $ \lat primQueryL ->
  Join LeftJoin cond (NonLateral, primQueryL) (lat, primQuery')

aProduct :: PrimQuery -> PrimQueryArr
aProduct pq = PrimQueryArr (\lat primQuery -> times lat primQuery pq)

aSemijoin :: SemijoinType -> PrimQuery -> PrimQueryArr
aSemijoin joint existsQ = PrimQueryArr $ \_ primQ -> Semijoin joint primQ existsQ

aRebind :: Bindings HPQ.PrimExpr -> PrimQueryArr
aRebind bindings = PrimQueryArr $ \_ -> Rebind True bindings

aRebindNoStar :: Bindings HPQ.PrimExpr -> PrimQueryArr
aRebindNoStar bindings = PrimQueryArr $ \_ -> Rebind False bindings

aRestrict :: HPQ.PrimExpr -> PrimQueryArr
aRestrict predicate = PrimQueryArr $ \_ -> restrict predicate

aLabel :: String -> PrimQueryArr
aLabel l = PrimQueryArr $ \_ primQ -> Label l primQ

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
--
-- Not every function of type `Lateral -> PrimQuery -> PrimQuery` is
-- valid to be a PrimQuery.  I think the condition that they must
-- satisfy for validity is
--
--     q == lateral (aProduct (toPrimQuery q)
--
-- where == is observable equivalence, i.e. both queries must give the
-- same results when combined with other queries and then run.
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
-- for emptiness explicitly in the SQL generation phase.

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
                  | Aggregate (Bindings (HPQ.Aggregate' HPQ.Symbol))
                              (PrimQuery' a)
                  | Window (Bindings (HPQ.WndwOp, HPQ.Partition)) (PrimQuery' a)
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
                  | RelExpr   HPQ.PrimExpr [Symbol]
                  | Rebind    Bool
                              (Bindings HPQ.PrimExpr)
                              (PrimQuery' a)
                  | ForUpdate (PrimQuery' a)
                  -- We may support more locking clauses than just
                  -- ForUpdate in the future
                  --
                  -- https://www.postgresql.org/docs/current/sql-select.html#SQL-FOR-UPDATE-SHARE
                  | With Recursive (Maybe Materialized) Symbol [Symbol] (PrimQuery' a) (PrimQuery' a)
                 deriving Show

type PrimQuery = PrimQuery' ()
type PrimQueryFold p = PrimQueryFold' () p

type PrimQueryFold' a p = PrimQueryFoldP a p p

data PrimQueryFoldP a p p' = PrimQueryFold
  { unit              :: p'
  , empty             :: a -> p'
  , baseTable         :: TableIdentifier -> Bindings HPQ.PrimExpr -> p'
  , product           :: NEL.NonEmpty (Lateral, p) -> [HPQ.PrimExpr] -> p'
  , aggregate         :: Bindings (HPQ.Aggregate' HPQ.Symbol)
                      -> p
                      -> p'
  , window            :: Bindings (HPQ.WndwOp, HPQ.Partition) -> p -> p'
  , distinctOnOrderBy :: Maybe (NEL.NonEmpty HPQ.PrimExpr)
                      -> [HPQ.OrderExpr]
                      -> p
                      -> p'
  , limit             :: LimitOp -> p -> p'
  , join              :: JoinType
                      -> HPQ.PrimExpr
                      -> (Lateral, p)
                      -> (Lateral, p)
                      -> p'
  , semijoin          :: SemijoinType -> p -> p -> p'
  , exists            :: Symbol -> p -> p'
  , values            :: [Symbol] -> NEL.NonEmpty [HPQ.PrimExpr] -> p'
  , binary            :: BinOp
                      -> (p, p)
                      -> p'
  , label             :: String -> p -> p'
  , relExpr           :: HPQ.PrimExpr -> [Symbol] -> p'
    -- ^ A relation-valued expression
  , rebind            :: Bool -> Bindings HPQ.PrimExpr -> p -> p'
  , forUpdate         :: p -> p'
  , with              :: Recursive -> Maybe Materialized -> Symbol -> [Symbol] -> p -> p -> p'
  }


primQueryFoldDefault :: PrimQueryFold' a (PrimQuery' a)
primQueryFoldDefault = PrimQueryFold
  { unit              = Unit
  , empty             = Empty
  , baseTable         = BaseTable
  , product           = Product
  , aggregate         = Aggregate
  , window            = Window
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
  , with              = With
  }

dimapPrimQueryFold :: (q -> p)
                   -> (p' -> q')
                   -> PrimQueryFoldP a p p'
                   -> PrimQueryFoldP a q q'
dimapPrimQueryFold self g f = PrimQueryFold
  { unit = g (unit f)
  , empty = g . empty f
  , baseTable = \ti bs -> g (baseTable f ti bs)
  , product = \ps conds -> g (product f ((fmap . fmap) self ps) conds)
  , aggregate = \b p -> g (aggregate f b (self p))
  , window = \b p -> g (window f b (self p))
  , distinctOnOrderBy = \m os p -> g (distinctOnOrderBy f m os (self p))
  , limit = \l p -> g (limit f l (self p))
  , join = \j pe lp lp' -> g (join f j pe (fmap self lp) (fmap self lp'))
  , semijoin = \j p1 p2 -> g (semijoin f j (self p1) (self p2))
  , exists = \s p -> g (exists f s (self p))
  , values = \ss nel -> g (values f ss nel)
  , binary = \bo (p1, p2) -> g (binary f bo (self p1, self p2))
  , label = \l p -> g (label f l (self p))
  , relExpr = \pe bs -> g (relExpr f pe bs)
  , rebind = \s bs p -> g (rebind f s bs (self p))
  , forUpdate = \p -> g (forUpdate f (self p))
  , with = \r m s ss p1 p2 -> g (with f r m s ss (self p1) (self p2))
  }

applyPrimQueryFoldF ::
  PrimQueryFoldP a (PrimQuery' a) p -> PrimQuery' a -> p
applyPrimQueryFoldF f = \case
  Unit -> unit f
  Empty a -> empty f a
  BaseTable ti syms -> baseTable f ti syms
  Product qs pes -> product f qs pes
  Aggregate aggrs q -> aggregate f aggrs q
  Window wndws q -> window f wndws q
  DistinctOnOrderBy dxs oxs q -> distinctOnOrderBy f dxs oxs q
  Limit op q -> limit f op q
  Join j cond q1 q2 -> join f j cond q1 q2
  Semijoin j q1 q2 -> semijoin f j q1 q2
  Values ss pes -> values f ss pes
  Binary binop (q1, q2) -> binary f binop (q1, q2)
  Label l pq -> label f l pq
  RelExpr pe syms -> relExpr f pe syms
  Exists s q -> exists f s q
  Rebind star pes q -> rebind f star pes q
  ForUpdate q -> forUpdate f q
  With recursive materialized name cols a b -> with f recursive materialized name cols a b

primQueryFoldF ::
  PrimQueryFoldP a p p' -> (PrimQuery' a -> p) -> PrimQuery' a -> p'
primQueryFoldF g self = applyPrimQueryFoldF (dimapPrimQueryFold self id g)

foldPrimQuery :: PrimQueryFold' a p -> PrimQuery' a -> p
foldPrimQuery f = fix (primQueryFoldF f)
  where fix g = let x = g x in x

-- Would be nice to show that this is associative
composePrimQueryFold ::
  PrimQueryFoldP a (PrimQuery' a) q ->
  PrimQueryFoldP a p (PrimQuery' a) ->
  PrimQueryFoldP a p q
composePrimQueryFold = fmapPrimQueryFold . applyPrimQueryFoldF
  where fmapPrimQueryFold = dimapPrimQueryFold id

times :: Lateral -> PrimQuery -> PrimQuery -> PrimQuery
times lat q q' = Product (pure q NEL.:| [(lat, q')]) []

restrict :: HPQ.PrimExpr -> PrimQuery -> PrimQuery
restrict cond primQ = Product (return (pure primQ)) [cond]

isUnit :: PrimQuery' a -> Bool
isUnit Unit = True
isUnit _    = False
