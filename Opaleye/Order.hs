module Opaleye.Order (module Opaleye.Order, O.OrderSpec) where

import qualified Opaleye.Column as C
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import qualified Opaleye.Internal.Order as O
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Database.HaskellDB.PrimQuery as HPQ

orderBy :: O.OrderSpec a -> Query a -> Query a
orderBy os q =
  Q.simpleQueryArr (O.orderByU os . Q.runSimpleQueryArr q)

desc :: (a -> C.Column b) -> O.OrderSpec a
desc = O.orderSpec HPQ.OpDesc

asc :: (a -> C.Column b) -> O.OrderSpec a
asc = O.orderSpec HPQ.OpAsc

limit :: Int -> Query a -> Query a
limit n a = Q.simpleQueryArr (O.limit' n . Q.runSimpleQueryArr a)

offset :: Int -> Query a -> Query a
offset n a = Q.simpleQueryArr (O.offset' n . Q.runSimpleQueryArr a)
