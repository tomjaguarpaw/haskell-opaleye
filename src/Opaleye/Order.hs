{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Ordering, @LIMIT@ and @OFFSET@

module Opaleye.Order ( -- * Order by
                       orderBy
                     , O.Order
                     -- * Order direction
                     , asc
                     , desc
                     , ascNullsFirst
                     , descNullsLast
                     -- * Limit and offset
                     , limit
                     , offset
                     -- * Exact ordering
                     , O.exact
                     -- * Other
                     , PGOrd
                     , SqlOrd
                     ) where

import qualified Opaleye.Column as C
import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Order as O
import qualified Opaleye.SqlTypes as T

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- ^ We can probably disable ConstraintKinds and TypeSynonymInstances
-- when we move to Sql... instead of PG..

{-| Order the rows of a `Query` according to the `Order`.

@
import Data.Monoid ((\<\>))

\-- Order by the first column ascending.  When first columns are equal
\-- order by second column descending.
example :: 'Query' ('C.Column' 'T.SqlInt4', 'C.Column' 'T.SqlText')
        -> 'Query' ('C.Column' 'T.SqlInt4', 'C.Column' 'T.SqlText')
example = 'orderBy' ('asc' fst \<\> 'desc' snd)
@

-}
orderBy :: O.Order a -> Query a -> Query a
orderBy os q =
  Q.simpleQueryArr (O.orderByU os . Q.runSimpleQueryArr q)

-- | Specify an ascending ordering by the given expression.
--   (Any NULLs appear last)
asc :: SqlOrd b => (a -> C.Column b) -> O.Order a
asc = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpAsc
                          , HPQ.orderNulls     = HPQ.NullsLast }

-- | Specify an descending ordering by the given expression.
--   (Any NULLs appear first)
desc :: SqlOrd b => (a -> C.Column b) -> O.Order a
desc = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpDesc
                           , HPQ.orderNulls     = HPQ.NullsFirst }

-- | Specify an ascending ordering by the given expression.
--   (Any NULLs appear first)
ascNullsFirst :: SqlOrd b => (a -> C.Column b) -> O.Order a
ascNullsFirst = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpAsc
                                    , HPQ.orderNulls     = HPQ.NullsFirst }


-- | Specify an descending ordering by the given expression.
--   (Any NULLs appear last)
descNullsLast :: SqlOrd b => (a -> C.Column b) -> O.Order a
descNullsLast = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpDesc
                                    , HPQ.orderNulls     = HPQ.NullsLast }

-- * Limit and offset

{- |
Limit the results of the given query to the given maximum number of
items.

/WARNING:/ If you're planning on using limit/offset together please use
'offset' /before/ you use 'limit', e.g.:

@
limit 10 (offset 50 yourQuery)
@

This is because Opaleye applies OFFSET and LIMIT to the query separately.
The result of the query given above is the following, which will return
10 rows after skipping the first 50 (probably what you want).

@
SELECT * FROM (SELECT * FROM yourTable OFFSET 50) LIMIT 10
@

However, reversing the order of the limit\/offset will result in the
following, which will result in /no rows being returned/ (probably
not what you want).

@
SELECT * FROM (SELECT * FROM yourTable LIMIT 10) OFFSET 50
@
-}
limit :: Int -> Query a -> Query a
limit n a = Q.simpleQueryArr (O.limit' n . Q.runSimpleQueryArr a)

{- |
Offset the results of the given query by the given amount, skipping
that many result rows.

/WARNING:/ Please read the documentation of 'limit' before combining
'offset' with 'limit'.
-}
offset :: Int -> Query a -> Query a
offset n a = Q.simpleQueryArr (O.offset' n . Q.runSimpleQueryArr a)

-- * Other

-- | Typeclass for Postgres types which support ordering operations.
class PGOrd a where

type SqlOrd = PGOrd

instance PGOrd T.SqlBool
instance PGOrd T.SqlDate
instance PGOrd T.SqlFloat8
instance PGOrd T.SqlFloat4
instance PGOrd T.SqlInt8
instance PGOrd T.SqlInt4
instance PGOrd T.SqlInt2
instance PGOrd T.SqlNumeric
instance PGOrd T.SqlText
instance PGOrd T.SqlTime
instance PGOrd T.SqlTimestamptz
instance PGOrd T.SqlTimestamp
instance PGOrd T.SqlCitext
instance PGOrd T.SqlUuid
instance PGOrd a => PGOrd (C.Nullable a)
