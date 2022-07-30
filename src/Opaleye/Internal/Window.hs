-- https://www.postgresql.org/docs/current/tutorial-window.html#id-1.4.5.6.9.5
-- talks about partitions and window frames.  The window frame is the
-- way the elements of a partition are ordered for processing the
-- result row of each element of the partition.
--
-- So neither of these terms is suitable for the _whole thing_.
-- Perhaps the answer should be "Window"?  This is also attested by
-- the WINDOW declaration in a SELECT.

module Opaleye.Internal.Window where

import           Control.Applicative (Applicative, pure, (<*>), liftA2)
import           Data.Profunctor (lmap, Profunctor, dimap)
import           Data.Semigroup (Semigroup, (<>))

import qualified Opaleye.Internal.Aggregate as A
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.Order as O

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ


newtype WindowFunction1 a =
  WindowFunction1 (PM.PackMap (HPQ.WndwOp, Window1) HPQ.PrimExpr () a)


-- | 'WindowFunction' represents expressions that contain [window
-- functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- You can choose a 'WindowFunction' from the options below, and
-- combine and manipulate them using the @Applicative@ and
-- 'Data.Profunctor.Profunctor' operations.
newtype WindowFunction a b = WindowFunction (a -> WindowFunction1 b)

instance Functor (WindowFunction a) where
  fmap f (WindowFunction w) = WindowFunction ((fmap . fmap) f w)

instance Applicative (WindowFunction a) where
  pure = WindowFunction . pure . pure
  WindowFunction f <*> WindowFunction x = WindowFunction (liftA2 (<*>) f x)

instance Profunctor WindowFunction where
  dimap f g (WindowFunction w) = WindowFunction (dimap f (fmap g) w)

-- | You can create @Windows@ using 'over', and combine and manipulate
-- them using the @Applicative@ and 'Data.Profunctor.Profunctor'
-- operations.
newtype Windows a b = Windows (a -> WindowFunction1 b)

instance Functor (Windows a) where
  fmap f (Windows w) = Windows ((fmap . fmap) f w)

instance Applicative (Windows a) where
  pure = Windows . pure . pure
  Windows f <*> Windows x = Windows (liftA2 (<*>) f x)

instance Profunctor Windows where
  dimap f g (Windows w) = Windows (dimap f (fmap g) w)

instance Functor WindowFunction1 where
  fmap f (WindowFunction1 g) = WindowFunction1 (fmap f g)

instance Applicative WindowFunction1 where
  pure = WindowFunction1 . pure
  WindowFunction1 f <*> WindowFunction1 x = WindowFunction1 (f <*> x)

runWindowFunction1 :: Applicative f
  => WindowFunction1 a -> ((HPQ.WndwOp, Window1) -> f HPQ.PrimExpr) -> f a
runWindowFunction1 (WindowFunction1 a) f = PM.traversePM a f ()


extractWindowFields
  :: T.Tag
  -> (HPQ.WndwOp, Window1)
  -> PM.PM (PQ.Bindings (HPQ.WndwOp, HPQ.Partition)) HPQ.PrimExpr
extractWindowFields tag (op, Window1 ps os) = do
  i <- PM.new
  let symbol = HPQ.Symbol ("window" ++ i) tag
  PM.write (symbol, (op, HPQ.Partition ps os))
  pure (HPQ.AttrExpr symbol)


-- | A 'WindowFunction' that doesn't actually contain any window
-- function.
noWindowFunction :: (a -> b) -> WindowFunction a b
noWindowFunction f = fmap f (WindowFunction pure)


window' :: Q.Select (WindowFunction1 a) -> Q.Select a
window' q = Q.productQueryArr $ do
  (wndw, primQ) <- Q.runSimpleQueryArr' q ()
  tag <- T.fresh
  let
    (a, bindings) = PM.run (runWindowFunction1 wndw (extractWindowFields tag))
  pure (a, PQ.Window bindings primQ)


-- | @runWindows@ runs a query composed of expressions containing
-- [window
-- functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- @runWindows@ is similar to 'Opaleye.aggregate', with the main
-- difference being that in a window query, each input row corresponds
-- to one output row, whereas aggregation queries fold the entire
-- input query down into a single row per group. In Haskell
-- terminology, 'Opaleye.aggregate' is to 'foldl' as @runWindows@ is
-- to 'scanl'.
runWindows :: Windows a b -> Q.Select a -> Q.Select b
runWindows (Windows w) = window' . fmap w


makeWndw :: WindowFunction HPQ.WndwOp (C.Field_ n a)
makeWndw = WindowFunction (\op -> WindowFunction1 (PM.PackMap (\f _ -> C.Column <$> f (op, mempty))))


makeWndwField :: (HPQ.PrimExpr -> HPQ.WndwOp)
              -> WindowFunction (C.Field_ n a) (C.Field_ n' a')
makeWndwField f = lmap (f . C.unColumn) makeWndw


makeWndwAny :: HPQ.WndwOp -> WindowFunction a (C.Field_ n b)
makeWndwAny op = lmap (const op) makeWndw

-- | 'aggregatorWindowFunction' allows the use of 'A.Aggregator's in
-- 'WindowFunction's. In particular, @'aggregatorWindowFunction'
-- 'Opaleye.sum'@ gives a running total (when combined with an order
-- argument to 'over').
aggregatorWindowFunction :: A.Aggregator a b -> (a' -> a) -> WindowFunction a' b
aggregatorWindowFunction agg g = WindowFunction $ \a -> WindowFunction1 $ PM.PackMap $ \f _ ->
  pm (\(mop, expr) -> case mop of
         Nothing -> pure expr
         Just (op, _, _) -> f (HPQ.WndwAggregate op expr, mempty)) a
  where A.Aggregator (PM.PackMap pm) = lmap g agg


-- | 'over' applies a 'WindowFunction' on a particular 'Window'.  For
-- example,
--
-- @
-- over ('aggregatorWindowFunction' 'Opaleye.sum' salary) ('partitionBy' department) ('Opaleye.desc' salary)
-- @
--
-- If you want to use a 'Window' that consists of the entire @SELECT@
-- then supply 'mempty' for the @'Window' a@ argument.  If you don't
-- want to order the 'Window' then supply 'mempty' for the @'O.Order'
-- a@ argument.
over :: WindowFunction a b -> Window a -> O.Order a -> Windows a b
over (WindowFunction windowFunction) (Window partition) order = Windows $ \a ->
  let WindowFunction1 (PM.PackMap pm) = windowFunction a
      Window orderPartitionBy' = orderPartitionBy order
  in  WindowFunction1 $ PM.PackMap $ \f -> pm $ \(op, partition') ->
    f (op, partition' <> partition a <> orderPartitionBy' a)

data Window1 = Window1 [HPQ.PrimExpr] [HPQ.OrderExpr]

-- | In PostgreSQL, window functions must specify the \"window\" over
-- which they operate. The syntax for this looks like: @SUM(salary)
-- OVER (PARTITION BY department)@. The Opaleye type 'Window'
-- represents the segment consisting of the @PARTIION BY@.
--
-- You can create a @Window@ using 'partitionBy' and combine two
-- @Windows@ in a single one which combines the partition of both by
-- using '<>'.
newtype Window a = Window (a -> Window1)

instance Semigroup (Window a) where
  Window w1 <> Window w2 = Window (w1 <> w2)

instance Monoid (Window a) where
  mempty = Window mempty
  mappend = (<>)

instance Semigroup Window1 where
  Window1 ps os <> Window1 ps' os' = Window1 (ps <> ps') (os <> os')

instance Monoid Window1 where
  mempty = Window1 [] []
  mappend = (<>)


-- | The window where each partition shares the same value for the
-- given 'Field'.
partitionBy :: (a -> C.Field_ n b) -> Window a
partitionBy f = Window $ \a ->
  let C.Column expr = f a
  in Window1 [expr] []

-- | Controls the order in which rows are processed by window functions. This
-- does not need to match the ordering of the overall query.
orderPartitionBy :: O.Order a -> Window a
orderPartitionBy ordering = Window $ \a -> Window1 [] (O.orderExprs a ordering)
