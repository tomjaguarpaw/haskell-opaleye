module Opaleye.Order (module Opaleye.Order, O.Order) where

import qualified Opaleye.Column as C
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import qualified Opaleye.Internal.Order as O

import qualified Database.HaskellDB.PrimQuery as HPQ

{-| Order the rows of a `Query` according to the `Order`. -}
orderBy :: O.Order a -> Query a -> Query a
orderBy os q =
  Q.simpleQueryArr (O.orderByU os . Q.runSimpleQueryArr q)

-- | Specify an ascending ordering by the given expression.
asc :: (a -> C.Column b) -> O.Order a
asc = O.order HPQ.OpAsc

-- | Specify an descending ordering by the given expression.
desc :: (a -> C.Column b) -> O.Order a
desc = O.order HPQ.OpDesc

{- |
Limit the results of the given query to the given maximum number of
items.
-}
limit :: Int -> Query a -> Query a
limit n a = Q.simpleQueryArr (O.limit' n . Q.runSimpleQueryArr a)

{- |
Offset the results of the given query by the given amount, skipping
that many result rows.
-}
offset :: Int -> Query a -> Query a
offset n a = Q.simpleQueryArr (O.offset' n . Q.runSimpleQueryArr a)
