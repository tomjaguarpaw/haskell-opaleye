module Opaleye.Lateral where

import           Opaleye.Select ( Select, SelectArr )
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.PrimQuery as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import           Control.Category ( (<<<) )


-- | Runs a subquery laterally, i.e. using SQL's @LATERAL@.
--
-- You might find 'laterally' and 'bilaterally' more convenient to use.
lateral :: (i -> Select a) -> SelectArr i a
lateral = O.lateral


-- | Allows to lift operations like 'Opaleye.Aggregate.aggregate',
-- 'Opaleye.Order.orderBy' and 'Opaleye.Order.limit', which are normally
-- restricted to 'Select', to operate on 'SelectArr's taking arbitrary inputs.
laterally :: (Select a -> Select b) -> SelectArr i a -> SelectArr i b
laterally f as = lateral (\i -> f (as <<< pure i))


-- | Allows to lift operations like 'Opaleye.Binary.union',
-- 'Opaleye.Binary.intersect' and 'Opaleye.Binary.except', which are
-- normally restricted to 'Select', to operate on 'SelectArr's taking
-- arbitrary inputs.
bilaterally :: (Select a -> Select b -> Select c)
            -> SelectArr i a -> SelectArr i b -> SelectArr i c
bilaterally f as bs = lateral (\i -> f (as <<< pure i) (bs <<< pure i))
