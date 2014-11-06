module Opaleye.Internal.Distinct where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Database.HaskellDB.PrimQuery as HPQ

-- FIXME: Need to use the tag.  Write a test for this!
distinctU :: U.Unpackspec columns columns'
          -> (columns, PQ.PrimQuery, T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
distinctU unpack (columns, primQ, t) = (newColumns, primQ', t)
  where (newColumns, groupPEs) =
          PM.run (U.runUnpackspec unpack extractAggregateFields columns)

        primQ' = PQ.Aggregate groupPEs primQ

extractAggregateFields :: HPQ.PrimExpr
      -> PM.PM [(String, Maybe HPQ.AggrOp, HPQ.PrimExpr)] HPQ.PrimExpr
extractAggregateFields pe = do
  i <- PM.new
  let s = "result" ++ i
  PM.write (s, Nothing, pe)
  return (HPQ.AttrExpr s)
