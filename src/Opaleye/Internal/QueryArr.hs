{-# LANGUAGE Arrows #-}

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
import           Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

-- Ideally this should be wrapped in a monad which automatically
-- increments the Tag, but I couldn't be bothered to do that.
newtype QueryArr a b = QueryArr ((a, PQ.PrimQuery, Tag) -> (b, PQ.PrimQuery, Tag))

type Query = QueryArr ()

productQueryArr :: ((a, Tag) -> (b, PQ.PrimQuery, Tag)) -> QueryArr a b
productQueryArr f = QueryArr g
  where g (a0, primQuery, t0) = (a1, PQ.times primQuery primQuery', t1)
          where (a1, primQuery', t1) = f (a0, t0)

{-# DEPRECATED simpleQueryArr "Use 'productQueryArr' instead. Its name indicates better what it actually does" #-}
simpleQueryArr :: ((a, Tag) -> (b, PQ.PrimQuery, Tag)) -> QueryArr a b
simpleQueryArr = productQueryArr

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

-- | A @SELECT@, i.e. an SQL query which produces a collection of
-- rows.
--
-- @Select a@ is analogous to a Haskell value @[a]@.
type Select = SelectArr ()

-- | A parametrised 'Select'.  A @SelectArr a b@ accepts an argument
-- of type @a@.
--
-- @SelectArr a b@ is analogous to a Haskell function @a -> [b]@.
type SelectArr = QueryArr

-- | Implements @LATERAL@ subqueries.
--
-- @LATERAL@ gives us goodies like 'Monad' and 'Control.Arrow.ArrowApply'
-- instances for 'SelectArr'. It also allows us to bypass the scoping rules
-- of SQL that otherwise restrict operations like
-- 'Opaleye.Aggregate.aggregate' and 'Opaleye.Binary.union' to 'Select's
-- rather than 'SelectArr's.
lateral :: (i -> Select a) -> SelectArr i a
lateral f = QueryArr qa
  where
    qa (i, primQueryL, tag) = (a, primQueryJoin, tag')
      where
        (a, primQueryR, tag') = runSimpleQueryArr (f i) ((), tag)
        primQueryJoin = PQ.Product ((PQ.NonLateral, primQueryL)
                                    :| [(PQ.Lateral, primQueryR)])
                                   []

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
  id = QueryArr id
  QueryArr f . QueryArr g = QueryArr (f . g)

instance Arr.Arrow QueryArr where
  arr f   = QueryArr (first3 f)
  first f = QueryArr g
    where g ((b, d), primQ, t0) = ((c, d), primQ', t1)
            where (c, primQ', t1) = runQueryArr f (b, primQ, t0)

instance Arr.ArrowChoice QueryArr where
  left f = QueryArr g
    where g (e, primQ, t0) = case e of
            Left a -> first3 Left (runQueryArr f (a, primQ, t0))
            Right b -> (Right b, primQ, t0)

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
