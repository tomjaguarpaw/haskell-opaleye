{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Lateral
  ( lateral
  , laterally
  , bilaterally
  )
where

import qualified Opaleye.Internal.QueryArr as Q
import           Opaleye.Select

import           Control.Category ( (<<<) )


-- | Implements @LATERAL@ subqueries.
--
-- @LATERAL@ gives us goodies like 'Monad' and 'Control.Arrow.ArrowApply'
-- instances for 'SelectArr'. It also allows us to bypass the scoping rules
-- of SQL that otherwise restrict operations like
-- 'Opaleye.Aggregate.aggregate' and 'Opaleye.Binary.union' to 'Select's
-- rather than 'SelectArr's.
lateral :: (i -> Select a) -> SelectArr i a
lateral = Q.lateral


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
