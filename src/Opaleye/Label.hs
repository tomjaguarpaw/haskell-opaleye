module Opaleye.Label where

import qualified Opaleye.Internal.Label as L
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Select            as S

-- | Add a commented label to the generated SQL.
label :: String -> S.SelectArr a b -> S.SelectArr a b
label l a = Q.QueryArr (L.label' l . Q.runQueryArr a)
