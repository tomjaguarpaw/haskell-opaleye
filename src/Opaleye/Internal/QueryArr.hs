module Opaleye.Internal.QueryArr where

import           Prelude hiding (id)

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as Tag
import           Opaleye.Internal.Tag (Tag)
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Control.Arrow as Arr
import           Control.Arrow ((&&&), (***), arr)
import qualified Control.Category as C
import           Control.Category ((<<<), id)
import           Control.Applicative (Applicative, pure, (<*>))
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

newtype QueryArr a b = QueryArr ((a, PQ.PrimQuery, Tag) -> (b, PQ.PrimQuery, Tag))
type Query = QueryArr ()

simpleQueryArr :: ((a, Tag) -> (b, PQ.PrimQuery, Tag)) -> QueryArr a b
simpleQueryArr f = QueryArr g
  where g (a0, primQuery, t0) = (a1, PQ.times primQuery primQuery', t1)
          where (a1, primQuery', t1) = f (a0, t0)

runQueryArr :: QueryArr a b -> (a, PQ.PrimQuery, Tag) -> (b, PQ.PrimQuery, Tag)
runQueryArr (QueryArr f) = f

runSimpleQueryArr :: QueryArr a b -> (a, Tag) -> (b, PQ.PrimQuery, Tag)
runSimpleQueryArr f (a, t) = runQueryArr f (a, PQ.Unit, t)

runSimpleQueryArrStart :: QueryArr a b -> a -> (b, PQ.PrimQuery, Tag)
runSimpleQueryArrStart q a = runSimpleQueryArr q (a, Tag.start)

runQueryArrUnpack :: U.Unpackspec a b
                  -> Query a -> ([HPQ.PrimExpr], PQ.PrimQuery, Tag)
runQueryArrUnpack unpackspec q = (primExprs, primQ, endTag)
  where (columns, primQ, endTag) = runSimpleQueryArrStart q ()
        primExprs = U.collectPEs unpackspec columns

first3 :: (a1 -> b) -> (a1, a2, a3) -> (b, a2, a3)
first3 f (a1, a2, a3) = (f a1, a2, a3)

instance C.Category QueryArr where
  id = QueryArr id
  QueryArr f . QueryArr g = QueryArr (f . g)

instance Arr.Arrow QueryArr where
  arr f   = QueryArr (first3 f)
  first f = QueryArr g
    where g ((b, d), primQ, t0) = ((c, d), primQ', t1)
            where (c, primQ', t1) = runQueryArr f (b, primQ, t0)

instance Functor (QueryArr a) where
  fmap f = (arr f <<<)

instance Applicative (QueryArr a) where
  pure = arr . const
  f <*> g = arr (uncurry ($)) <<< (f &&& g)

instance P.Profunctor QueryArr where
  dimap f g a = arr g <<< a <<< arr f

instance PP.ProductProfunctor QueryArr where
  empty = id
  (***!) = (***)
