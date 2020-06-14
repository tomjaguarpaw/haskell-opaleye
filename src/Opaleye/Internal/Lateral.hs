{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Internal.Lateral
  ( lateral
  , laterally
  , bilaterally
  )
where

import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.Select

import           Data.List.NonEmpty ( NonEmpty((:|)) )
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

-- | Lifts operations like 'Opaleye.Aggregate.aggregate',
-- 'Opaleye.Order.orderBy' and 'Opaleye.Order.limit', which are restricted to
-- 'Select' normally, to operate on 'SelectArr's taking arbitrary inputs.
laterally :: (Select a -> Select b) -> SelectArr i a -> SelectArr i b
laterally f as = lateral (\i -> f (as <<< pure i))


-- | Lifts operations like 'Opaleye.Binary.union', 'Opaleye.Binary.intersect'
-- and 'Opaleye.Binary.except', which are restricted to 'Select' normally, to
-- operate on 'SelectArr's taking arbitrary inputs.
bilaterally :: (Select a -> Select b -> Select c)
            -> SelectArr i a -> SelectArr i b -> SelectArr i c
bilaterally f as bs = lateral (\i -> f (as <<< pure i) (bs <<< pure i))
