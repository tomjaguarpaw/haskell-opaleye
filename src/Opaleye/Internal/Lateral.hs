{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Opaleye.Internal.Lateral
  ( lateral
  , viaLateral
  , laterally
  , bilaterally
  , bind
  , arrowApply
  )
where

import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.Select

import           Data.List.NonEmpty ( NonEmpty((:|)) )
import           Control.Arrow ( returnA )
import           Control.Category ( (<<<) )


-- | Implements @LATERAL@ subqueries.
--
-- @LATERAL@ gives us goodies like 'Monad' and 'Control.Arrow.ArrowApply'
-- instances for 'SelectArr'. It also allows us to bypass the scoping rules
-- of SQL that otherwise restrict operations like
-- 'Opaleye.Aggregate.aggregate' and 'Opaleye.Binary.union' to 'Select's
-- rather than 'SelectArr's.
lateral :: (i -> Select a) -> SelectArr i a
lateral f = Q.QueryArr qa
  where
    qa (i, primQueryL, tag) = (a, primQueryJoin, tag')
      where
        (a, primQueryR, tag') = Q.runSimpleQueryArr (f i) ((), tag)
        primQueryJoin = PQ.Product ((PQ.NonLateral, primQueryL)
                                    :| [(PQ.Lateral, primQueryR)])
                                   []

viaLateral :: SelectArr i a -> i -> Select a
viaLateral s i = s <<< pure i

-- | Lifts operations like 'Opaleye.Aggregate.aggregate',
-- 'Opaleye.Order.orderBy' and 'Opaleye.Order.limit', which are restricted to
-- 'Select' normally, to operate on 'SelectArr's taking arbitrary inputs.
laterally :: (Select a -> Select b) -> SelectArr i a -> SelectArr i b
laterally f as = lateral (\i -> f (viaLateral as i))


-- | Lifts operations like 'Opaleye.Binary.union', 'Opaleye.Binary.intersect'
-- and 'Opaleye.Binary.except', which are restricted to 'Select' normally, to
-- operate on 'SelectArr's taking arbitrary inputs.
bilaterally :: (Select a -> Select b -> Select c)
            -> SelectArr i a -> SelectArr i b -> SelectArr i c
bilaterally f as bs = lateral (\i -> f (viaLateral as i) (viaLateral bs i))

bind :: SelectArr i a -> (a -> SelectArr i b) -> SelectArr i b
bind s f = proc i -> do
  a <- s -< i
  b <- lateral (\(a, i) -> viaLateral (f a) i) -< (a, i)
  returnA -< b

arrowApply :: SelectArr (SelectArr i a, i) a
arrowApply = lateral (\(f, i) -> viaLateral f i)
