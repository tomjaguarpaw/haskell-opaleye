module Opaleye.Order (module Opaleye.Order, O.Order) where

import qualified Opaleye.Column as C
import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Order as O
import qualified Opaleye.PGTypes as T

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

{-| Order the rows of a `Query` according to the `Order`.

@
import Data.Monoid (\<\>)

\-- Order by the first column ascending.  When first columns are equal
\-- order by second column descending.
example :: 'Query' ('C.Column' 'T.PGInt4', 'C.Column' 'T.PGText')
        -> 'Query' ('C.Column' 'T.PGInt4', 'C.Column' 'T.PGText')
example = 'orderBy' ('asc' fst \<\> 'desc' snd)
@

-}
orderBy :: O.Order a -> Query a -> Query a
orderBy os q =
  Q.simpleQueryArr (O.orderByU os . Q.runSimpleQueryArr q)

-- | Specify an ascending ordering by the given expression.
--   (Any NULLs appear last)
asc :: PGOrd b => (a -> C.Column b) -> O.Order a
asc = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpAsc
                          , HPQ.orderNulls     = HPQ.NullsLast }

-- | Specify an descending ordering by the given expression.
--   (Any NULLs appear first)
desc :: PGOrd b => (a -> C.Column b) -> O.Order a
desc = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpDesc
                           , HPQ.orderNulls     = HPQ.NullsFirst }

-- | Specify an ascending ordering by the given expression.
--   (Any NULLs appear first)
ascNullsFirst :: PGOrd b => (a -> C.Column b) -> O.Order a
ascNullsFirst = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpAsc
                                    , HPQ.orderNulls     = HPQ.NullsFirst }


-- | Specify an descending ordering by the given expression.
--   (Any NULLs appear last)
descNullsLast :: PGOrd b => (a -> C.Column b) -> O.Order a
descNullsLast = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpDesc
                                    , HPQ.orderNulls     = HPQ.NullsLast }

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

-- | Typeclass for Postgres types which support ordering operations.
class PGOrd a where

instance PGOrd T.PGBool
instance PGOrd T.PGDate
instance PGOrd T.PGFloat8
instance PGOrd T.PGFloat4
instance PGOrd T.PGInt8
instance PGOrd T.PGInt4
instance PGOrd T.PGInt2
instance PGOrd T.PGNumeric
instance PGOrd T.PGText
instance PGOrd T.PGTime
instance PGOrd T.PGTimestamptz
instance PGOrd T.PGTimestamp
instance PGOrd T.PGCitext
