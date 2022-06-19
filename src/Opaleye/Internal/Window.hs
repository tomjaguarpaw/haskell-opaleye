module Opaleye.Internal.Window where

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Opaleye.Internal.Aggregate as A
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.Order as O

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ


-- | 'Window' is an applicative functor that represents expressions that
-- contain
-- [window functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- 'window' can be used to evaluate these expressions over a particular query.
newtype Window a =
  Window (PM.PackMap (HPQ.WndwOp, Partition) HPQ.PrimExpr () a)


instance Functor Window where
  fmap f (Window g) = Window (fmap f g)


instance Applicative Window where
  pure = Window . pure
  Window f <*> Window x = Window (f <*> x)


runWindow :: Applicative f
  => Window a -> ((HPQ.WndwOp, Partition) -> f HPQ.PrimExpr) -> f a
runWindow (Window a) f = PM.traversePM a f ()


extractWindowFields
  :: T.Tag
  -> (HPQ.WndwOp, Partition)
  -> PM.PM (PQ.Bindings (HPQ.WndwOp, HPQ.Partition)) HPQ.PrimExpr
extractWindowFields tag (op, Partition ps os) = do
  i <- PM.new
  let symbol = HPQ.Symbol ("window" ++ i) tag
  PM.write (symbol, (op, (HPQ.Partition ps os)))
  pure (HPQ.AttrExpr symbol)


-- | 'window' runs a query composed of expressions containing
-- [window functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- 'window' is similar to 'Opaleye.aggregate', with the main difference being
-- that in a window query, each input row corresponds to one output row,
-- whereas aggregation queries fold the entire input query down into a single
-- row. To put this into a Haskell context, 'Opaleye.aggregate' is to 'foldl'
-- as 'window' is to 'scanl'.
window :: Q.Select (Window a) -> Q.Select a
window q = Q.productQueryArr $ do
  (wndw, primQ) <- Q.runSimpleQueryArr' q ()
  tag <- T.fresh
  let
    (a, bindings) = PM.run (runWindow wndw (extractWindowFields tag))
  pure (a, PQ.Window bindings primQ)


makeWndw :: HPQ.WndwOp -> Window (C.Field_ n a)
makeWndw op = Window (PM.PackMap (\f _ -> C.Column <$> f (op, mempty)))


-- | 'cumulative' allows the use of aggregation functions in 'Window'
-- expressions. In particular, @'cumulative' 'Opaleye.sum'@
-- (when combined with 'orderPartitionBy') gives a running total,
-- also known as a \"cumulative sum\", hence the name @cumulative@.
cumulative :: A.Aggregator a b -> a -> Window b
cumulative (A.Aggregator (PM.PackMap pm)) a = Window $ PM.PackMap $ \f _ ->
  pm (\(mop, expr) -> case mop of
         Nothing -> pure expr
         Just (op, _, _) -> f (HPQ.WndwAggregate op expr, mempty)) a


-- | 'over' adds a 'Partition' to a 'Window' expression.
--
-- @
-- 'cumulative' 'Opaleye.sum' salary \`'over'\` 'partitionBy' department <> 'orderPartitionBy' salary ('Opaleye.desc' id)
-- @
over :: Window a -> Partition -> Window a
over (Window (PM.PackMap pm)) partition =
  Window $ PM.PackMap $ \f -> pm $ \(op, partition') ->
    f (op, partition' <> partition)
infixl 1 `over`


-- | In PostgreSQL, window functions must specify the \"window\" or
-- \"partition\" over which they operate. The syntax for this looks like:
-- @SUM(salary) OVER (PARTITION BY department)@. The Opaleye type 'Partition'
-- represents everything that comes after @OVER@.
--
-- 'Partition' is a 'Monoid', so 'Partition's created with 'partitionBy' and
-- 'orderPartitionBy' can be combined using '<>'.
data Partition = Partition ![HPQ.PrimExpr] ![HPQ.OrderExpr]


instance Semigroup Partition where
  Partition ps os <> Partition ps' os' = Partition (ps <> ps') (os <> os')


instance Monoid Partition where
  mempty = Partition [] []


-- | Restricts a window function to operate only the group of rows that share
-- the same value(s) for the given expression(s).
partitionBy :: C.Field_ n a -> Partition
partitionBy (C.Column expr) = Partition [expr] []


-- | Controls the order in which rows are processed by window functions. This
-- does not need to match the ordering of the overall query.
orderPartitionBy :: a -> O.Order a -> Partition
orderPartitionBy a ordering = Partition [] (O.orderExprs a ordering)
