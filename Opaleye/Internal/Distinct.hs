module Opaleye.Internal.Distinct where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Database.HaskellDB.PrimQuery as HPQ

import qualified Control.Monad.Trans.State as S

-- TODO: duplication with runQueryArrUnpack
-- FIXME: Need to use the tag.  Write a test for this!
distinctU :: U.Unpackspec columns columns'
          -> (columns, PQ.PrimQuery, T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
distinctU unpack (columns, primQ, t) = (newColumns, primQ', t)
  where -- TODO: This should really be a writer for the writable part
        (newColumns, (groupPEs', _)) =
          S.runState (U.runUnpackspec unpack extractAggregateFields columns) ([], 0)

        primQ' = PQ.Aggregate groupPEs' primQ

extractAggregateFields :: HPQ.PrimExpr
      -> S.State ([(String, Maybe HPQ.AggrOp, HPQ.PrimExpr)], Int) HPQ.PrimExpr
extractAggregateFields pe = do
          (groupPEs, i) <- S.get
          let s = "result" ++ show i
          S.put (groupPEs ++ [(s, Nothing, pe)], i+1)
          return (HPQ.AttrExpr s)
