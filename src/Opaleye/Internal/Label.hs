module Opaleye.Internal.Label where

import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ

label' :: String -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
label' l (x, q, t) = (x, PQ.Label l q, t)
