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
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import           Data.Semigroup ((<>))

-- Ideally this should be wrapped in a monad which automatically
-- increments the Tag, but I couldn't be bothered to do that.

-- | A parametrised 'Select'.  A @SelectArr a b@ accepts an argument
-- of type @a@.
--
-- @SelectArr a b@ is analogous to a Haskell function @a -> [b]@.
newtype SelectArr a b = QueryArr ((a, Tag) -> (b, PQ.PrimQueryArr, Tag))

type QueryArr = SelectArr
type Query = SelectArr ()

productQueryArr :: ((a, Tag) -> (b, PQ.PrimQuery, Tag)) -> QueryArr a b
productQueryArr f = QueryArr g
  where g (a0, t0) = (a1, PQ.PrimQueryArr (\lat primQuery -> PQ.times lat primQuery primQuery'), t1)
          where (a1, primQuery', t1) = f (a0, t0)

leftJoinQueryArr :: ((a, Tag) -> (b, HPQ.PrimExpr, PQ.PrimQuery, Tag)) -> QueryArr a b
leftJoinQueryArr f = QueryArr g
  where g (a0, t0) = (a1, PQ.PrimQueryArr $ \lat primQueryL ->
                            PQ.Join PQ.LeftJoin cond (PQ.NonLateral, primQueryL) (lat, primQuery'), t1)
          where (a1, cond, primQuery', t1) = f (a0, t0)

runQueryArr :: QueryArr a b -> (a, Tag) -> (b, PQ.PrimQueryArr, Tag)
runQueryArr (QueryArr f) = f

runSimpleQueryArr :: QueryArr a b -> (a, Tag) -> (b, PQ.PrimQuery, Tag)
runSimpleQueryArr f = (\(b, pqf, t) -> (b, PQ.toPrimQuery pqf, t)) . runQueryArr f

runSimpleQueryArrStart :: QueryArr a b -> a -> (b, PQ.PrimQuery, Tag)
runSimpleQueryArrStart q a = runSimpleQueryArr q (a, Tag.start)

runQueryArrUnpack :: U.Unpackspec a b
                  -> Query a -> ([HPQ.PrimExpr], PQ.PrimQuery, Tag)
runQueryArrUnpack unpackspec q = (primExprs, primQ, endTag)
  where (columns, primQ, endTag) = runSimpleQueryArrStart q ()
        primExprs = U.collectPEs unpackspec columns

first3 :: (a1 -> b) -> (a1, a2, a3) -> (b, a2, a3)
first3 f (a1, a2, a3) = (f a1, a2, a3)

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
lateral f = QueryArr qa
  where
    qa (i, tag) = (a, primQueryJoin, tag')
      where
        (a, primQueryR, tag') = runQueryArr (f i) ((), tag)
        primQueryJoin = PQ.lateral primQueryR

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
  QueryArr f . QueryArr g = QueryArr (\(a, t) ->
                                        let (b, pqf, t') = g (a, t)
                                            (c, pqf', t'') = f (b, t')
                                        in (c, pqf <> pqf', t''))

instance Arr.Arrow QueryArr where
  arr f   = QueryArr (\(a, t) -> (f a, mempty, t))
  first (QueryArr f) = QueryArr g
    where g ((b, d), t0) = first3 (\c -> (c, d)) (f (b, t0))

instance Arr.ArrowChoice QueryArr where
  left (QueryArr f) = QueryArr g
    where g (e, t0) = case e of
            Left a -> first3 Left (f (a, t0))
            Right b -> (Right b, mempty, t0)

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
