module Opaleye.Order (module Opaleye.Order, O.OrderSpec) where

import qualified Opaleye.Column as C
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import qualified Opaleye.Internal.Order as O

import qualified Database.HaskellDB.PrimQuery as PQ

orderBy :: O.OrderSpec a -> Query a -> Query a
orderBy os q =
  Q.simpleQueryArr (O.orderByU os . Q.runSimpleQueryArr q)

desc :: (a -> C.Column b) -> O.OrderSpec a
desc = O.orderSpec PQ.OpDesc

asc :: (a -> C.Column b) -> O.OrderSpec a
asc = O.orderSpec PQ.OpAsc
