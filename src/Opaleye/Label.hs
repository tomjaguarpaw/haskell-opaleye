module Opaleye.Label (
  label
  ) where

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Select            as S

-- | Add a commented label to the generated SQL.
label :: String -> S.SelectArr a b -> S.SelectArr a b
label l s = Q.QueryArr ((\(a, pqf, t') -> (a, \t -> PQ.Label l . pqf t, t'))
                         . Q.runQueryArr s)
