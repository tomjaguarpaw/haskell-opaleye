module Opaleye.Label where

import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.Label as L
import qualified Opaleye.Internal.QueryArr as Q

-- | Add a commented label to the generated SQL.
label :: String -> Query a -> Query a
label l a = Q.simpleQueryArr (L.label' l . Q.runSimpleQueryArr a)
