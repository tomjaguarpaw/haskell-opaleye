-- https://www.postgresql.org/docs/current/tutorial-window.html#id-1.4.5.6.9.5
-- talks about partitions and window frames.  The window frame is the
-- way the elements of a partition are ordered for processing the
-- result row of each element of the partition.
--
-- So neither of these terms is suitable for the _whole thing_.
-- Perhaps the answer should be "Window"?  This is also attested by
-- the WINDOW declaration in a SELECT.

{-# LANGUAGE LambdaCase #-}

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
import Data.Functor.Contravariant (contramap, Contravariant)
import Control.Arrow (second)


-- | 'WindowFunction' represents expressions that contain [window
-- functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- You can choose a 'WindowFunction' from the options below, and
-- combine and manipulate them using the @Applicative@ and
-- 'Data.Profunctor.Profunctor' operations.
newtype WindowFunction a b =
  WindowFunction (PM.PackMap HPQ.WndwOp HPQ.PrimExpr a b)

instance Functor (WindowFunction a) where
  fmap f (WindowFunction w) = WindowFunction (fmap f w)

instance Applicative (WindowFunction a) where
  pure = WindowFunction . pure
  WindowFunction f <*> WindowFunction x = WindowFunction ((<*>) f x)

instance Profunctor WindowFunction where
  dimap f g (WindowFunction w) =  WindowFunction (dimap f g w)

-- | You can create @Windows@ using 'over', and combine and manipulate
-- them using the @Applicative@ and 'Data.Profunctor.Profunctor'
-- operations.
newtype Windows a b =
  Windows (PM.PackMap (HPQ.WndwOp, Window a) HPQ.PrimExpr a b)

instance Functor (Windows a) where
  fmap f (Windows w) = Windows (fmap f w)

instance Applicative (Windows a) where
  pure = Windows . pure
  Windows f <*> Windows x = Windows ((<*>) f x)

instance Profunctor Windows where
  dimap f g (Windows (PM.PackMap pm)) =
    Windows $ PM.PackMap $ \h a ->
      fmap g (pm (\(op, w) -> h (op, contramap f w)) (f a))

runWindows' :: Applicative f
  => Windows a b -> ((HPQ.WndwOp, Window a) -> f HPQ.PrimExpr) -> a -> f b
runWindows' (Windows a) = PM.traversePM a


extractWindowFields
  :: T.Tag
  -> a
  -> (HPQ.WndwOp, Window a)
  -> PM.PM (PQ.Bindings (HPQ.WndwOp, HPQ.Partition)) HPQ.PrimExpr
extractWindowFields tag a (op, Window ps os) = do
  i <- PM.new
  let symbol = HPQ.Symbol ("window" ++ i) tag
  PM.write (symbol, (op, HPQ.Partition (ps a) (O.orderExprs a os)))
  pure (HPQ.AttrExpr symbol)


-- | A 'WindowFunction' that doesn't actually contain any window
-- function.
noWindowFunction :: (a -> b) -> WindowFunction a b
noWindowFunction f = fmap f (WindowFunction (PM.PackMap (const pure)))


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
runWindows wndw q = Q.productQueryArr $ do
  (a, primQ) <- Q.runSimpleSelect q
  tag <- T.fresh
  let
    (b, bindings) = PM.run (runWindows' wndw (extractWindowFields tag a) a)
  pure (b, PQ.Window bindings primQ)


windowsApply :: Windows (Windows a b, a) b
windowsApply = Windows $ PM.PackMap $ \f (agg, a) ->
  case agg of
    Windows (PM.PackMap inner) -> inner (f . second (contramap snd)) a


makeWndw :: WindowFunction HPQ.WndwOp (C.Field_ n a)
makeWndw = WindowFunction (PM.PackMap (\f op -> C.Column <$> f op))


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
aggregatorWindowFunction agg g = WindowFunction $ PM.PackMap $ \f a ->
  pm (\case
         HPQ.GroupBy expr -> pure expr
         HPQ.Aggregate (HPQ.Aggr' op e _ _ _) -> f (HPQ.WndwAggregate op e)) a
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
over (WindowFunction windowFunction) partition order =
  let PM.PackMap pm = windowFunction
      orderPartitionBy' = orderPartitionBy order
  in Windows $ PM.PackMap $ \f -> pm (\op ->
    f (op, partition <> orderPartitionBy'))

-- | In PostgreSQL, window functions must specify the \"window\" over
-- which they operate. The syntax for this looks like: @SUM(salary)
-- OVER (PARTITION BY department)@. The Opaleye type 'Window'
-- represents the segment consisting of the @PARTIION BY@.
--
-- You can create a @Window@ using 'partitionBy' and combine two
-- @Windows@ in a single one which combines the partition of both by
-- using '<>'.
data Window a = Window (a -> [HPQ.PrimExpr]) (O.Order a)

instance Semigroup (Window a) where
  Window p1 o1 <> Window p2 o2 = Window (p1 <> p2) (o1 <> o2)

instance Monoid (Window a) where
  mempty = Window mempty mempty
  mappend = (<>)

instance Contravariant Window where
  contramap f (Window p o) = Window (lmap f p) (contramap f o)

-- | The window where each partition shares the same value for the
-- given 'Field'.
partitionBy :: (a -> C.Field_ n b) -> Window a
partitionBy f = Window (\a -> [C.unColumn (f a)]) mempty

-- | Controls the order in which rows are processed by window functions. This
-- does not need to match the ordering of the overall query.
orderPartitionBy :: O.Order a -> Window a
orderPartitionBy = Window mempty
