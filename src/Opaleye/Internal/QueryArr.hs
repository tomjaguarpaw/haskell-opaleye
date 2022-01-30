{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Opaleye.Internal.QueryArr where

import           Prelude hiding (id)

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as Tag
import           Opaleye.Internal.Tag (Tag)
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Control.Arrow as Arr
import           Control.Arrow ((&&&), (***), arr, returnA)
import qualified Control.Category as C
import           Control.Category ((<<<), id)
import           Control.Applicative (Applicative, pure, (<*>))
import           Control.Monad.Trans.State.Strict (State, get, put, runState)
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import           Data.Semigroup ((<>))

-- | A parametrised 'Select'.  A @SelectArr a b@ accepts an argument
-- of type @a@.
--
-- @SelectArr a b@ is analogous to a Haskell function @a -> [b]@.
newtype SelectArr a b = QueryArr { unQueryArr :: a -> State Tag (b, PQ.PrimQueryArr) }

type QueryArr = SelectArr
type Query = SelectArr ()

productQueryArr :: ((a, Tag) -> (b, PQ.PrimQuery, Tag)) -> QueryArr a b
productQueryArr f = QueryArr $ \a -> do
  t <- get
  let (b, pq, t') = f (a, t)
  put t'
  pure (b, PQ.PrimQueryArr (\lat primQuery -> PQ.times lat primQuery pq))

leftJoinQueryArr :: ((a, Tag) -> (b, HPQ.PrimExpr, PQ.PrimQuery, Tag)) -> QueryArr a b
leftJoinQueryArr f = leftJoinQueryArr' $ \a -> do
  t <- get
  let (a1, cond, primQuery', t1) = f (a, t)
  put t1
  pure (a1, cond, primQuery')

leftJoinQueryArr' :: (a -> State Tag (b, HPQ.PrimExpr, PQ.PrimQuery)) -> QueryArr a b
leftJoinQueryArr' f = QueryArr $ \a -> do
  (a1, cond, primQuery') <- f a
  pure (a1, PQ.PrimQueryArr $ \lat primQueryL ->
                            PQ.Join PQ.LeftJoin cond (PQ.NonLateral, primQueryL) (lat, primQuery'))

runSimpleQueryArr :: QueryArr a b -> (a, Tag) -> (b, PQ.PrimQuery, Tag)
runSimpleQueryArr f (a, t0) =
  let ((b, pqa), t') = runState (runSimpleQueryArr' f a) t0
  in (b, pqa, t')

runSimpleQueryArr' :: QueryArr a b -> a -> State Tag (b, PQ.PrimQuery)
runSimpleQueryArr' f a = do
  (b, pqf) <- unQueryArr f a
  pure (b, PQ.toPrimQuery pqf)

runSimpleQueryArrStart :: QueryArr a b -> a -> (b, PQ.PrimQuery, Tag)
runSimpleQueryArrStart q a = runSimpleQueryArr q (a, Tag.start)

runQueryArrUnpack :: U.Unpackspec a b
                  -> Query a -> ([HPQ.PrimExpr], PQ.PrimQuery, Tag)
runQueryArrUnpack unpackspec q = (primExprs, primQ, endTag)
  where (columns, primQ, endTag) = runSimpleQueryArrStart q ()
        primExprs = U.collectPEs unpackspec columns

-- | A @SELECT@, i.e. an SQL query which produces a collection of
-- rows.
--
-- @Select a@ is analogous to a Haskell value @[a]@.
type Select = SelectArr ()

-- | Implements @LATERAL@ subqueries.
--
-- You might find it easier to use 'Opaleye.Lateral.laterally' (if you
-- want to apply 'Opaleye.Aggregate.aggregate',
-- 'Opaleye.Order.orderBy' or 'Opaleye.Order.limit' to a 'SelectArr')
-- or 'Opaleye.Lateral.bilaterally' (if you want to apply
-- 'Opaleye.Binary.union', 'Opaleye.Binary.intersect' and
-- 'Opaleye.Binary.except' to two 'SelectArr's).
lateral :: (i -> Select a) -> SelectArr i a
lateral f = QueryArr $ \i -> do
  (a, primQueryR) <- unQueryArr (f i) ()
  let primQueryJoin = PQ.lateral primQueryR
  pure (a, primQueryJoin)

-- | Convert an arrow argument into a function argument so that it can
-- be applied inside @do@-notation rather than arrow notation.
viaLateral :: SelectArr i a -> i -> Select a
viaLateral s i = s <<< pure i

bind :: SelectArr i a -> (a -> SelectArr i b) -> SelectArr i b
bind s f = proc i -> do
  a <- s -< i
  b <- lateral (\(a, i) -> viaLateral (f a) i) -< (a, i)
  returnA -< b

arrowApply :: SelectArr (SelectArr i a, i) a
arrowApply = lateral (\(f, i) -> viaLateral f i)

instance C.Category QueryArr where
  id = arr id
  QueryArr f . QueryArr g = QueryArr $ \a -> do
    (b, pqf)  <- g a
    (c, pqf') <- f b
    pure (c, pqf <> pqf')

instance Arr.Arrow QueryArr where
  arr f   = QueryArr (\a -> pure (f a, mempty))
  first (QueryArr f) = QueryArr g
    where g (b, d) = do
            (c, pq) <- f b
            pure ((c, d), pq)

instance Arr.ArrowChoice QueryArr where
  left (QueryArr f) = QueryArr g
    where g e = case e of
            Left a -> do
              (r, pq) <- f a
              pure (Left r, pq)
            Right b -> pure (Right b, mempty)

instance Arr.ArrowApply QueryArr where
  app = arrowApply

instance Functor (QueryArr a) where
  fmap f = (arr f <<<)

instance Applicative (QueryArr a) where
  pure = arr . const
  f <*> g = arr (uncurry ($)) <<< (f &&& g)

instance Monad (QueryArr a) where
  return = pure
  (>>=) = bind

instance P.Profunctor QueryArr where
  dimap f g a = arr g <<< a <<< arr f

instance PP.ProductProfunctor QueryArr where
  empty = id
  (***!) = (***)
