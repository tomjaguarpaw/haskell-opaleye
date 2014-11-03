{-|

This modules defines the 'QueryArr' arrow, which is an arrow that represents
selecting data from a database, and composing multiple queries together.

-}
module Opaleye.QueryArr where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as Tag
import           Opaleye.Internal.Tag (Tag)

import qualified Database.HaskellDB.PrimQuery as PQ

newtype QueryArr a b = QueryArr ((a, PQ.PrimQuery, Tag) -> (b, PQ.PrimQuery, Tag))
type Query = QueryArr ()

simpleQueryArr :: ((a, Tag) -> (b, PQ.PrimQuery, Tag)) -> QueryArr a b
simpleQueryArr f = QueryArr g
  where g (a0, primQuery, t0) = (a1, PQ.times primQuery primQuery', t1)
          where (a1, primQuery', t1) = f (a0, t0)

runQueryArr :: QueryArr a b -> (a, PQ.PrimQuery, Tag) -> (b, PQ.PrimQuery, Tag)
runQueryArr (QueryArr f) = f

runSimpleQueryArr :: QueryArr a b -> (a, Tag) -> (b, PQ.PrimQuery, Tag)
runSimpleQueryArr f (a, t) = runQueryArr f (a, PQ.Empty, t)

runQueryArrUnpack :: U.Unpackspec a b -> Query a -> PQ.PrimQuery
runQueryArrUnpack unpackspec q = primQ'
  where (columns, primQ, _) = runSimpleQueryArr q ((), Tag.start)
        f pe = ([pe], ())
        primExprs :: [PQ.PrimExpr]
        (primExprs, _) = U.runUnpackspec unpackspec f columns

        attrs = map (\(i, primExpr) -> ("result" ++ show i, primExpr))
                    (zip [1 :: Int ..] primExprs)

        primQ' = PQ.Project attrs primQ
