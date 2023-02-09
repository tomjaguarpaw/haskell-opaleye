{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Internal.Lateral
  ( lateral
  , viaLateral
  , laterally
  , bilaterally
  , bind
  , arrowApply
  )
where

import           Opaleye.Internal.QueryArr

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
