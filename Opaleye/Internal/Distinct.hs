module Opaleye.Internal.Distinct where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Database.HaskellDB.PrimQuery as HPQ

distinctU :: U.Unpackspec columns columns'
          -> (columns, PQ.PrimQuery, T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
distinctU unpack (columns, primQ, t) = (newColumns, primQ', T.next t)
  where (newColumns, groupPEs) =
          PM.run (U.runUnpackspec unpack (extractAggregateFields t) columns)

        primQ' = PQ.Aggregate groupPEs primQ

-- TODO: Should this be in the singular?
extractAggregateFields :: T.Tag -> HPQ.PrimExpr
      -> PM.PM [(String, Maybe HPQ.AggrOp, HPQ.PrimExpr)] HPQ.PrimExpr
extractAggregateFields tag pe = do
  i <- PM.new
  let s = T.tagWith tag ("result" ++ i)
  PM.write (s, Nothing, pe)
  return (HPQ.AttrExpr s)
