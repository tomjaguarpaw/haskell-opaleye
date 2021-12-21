{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @ORDER BY@, @LIMIT@, @OFFSET@ and @DISTINCT ON@

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
                     -- * Distinct on
                     , distinctOn
                     , distinctOnBy
                     -- * Exact ordering
                     , O.exact
                     -- * Other
                     , SqlOrd
                     -- * Deprecated
                     , distinctOnCorrect
                     , distinctOnByCorrect
                     ) where

import qualified Data.Profunctor.Product.Default as D
import qualified Opaleye.Field as F
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.Order as O
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Select         as S
import qualified Opaleye.SqlTypes as T

-- We can probably disable ConstraintKinds and TypeSynonymInstances
-- when we move to Sql... instead of PG..

{-| Order the rows of a `S.Select` according to the `O.Order`.

@
import Data.Monoid ((\<\>))

\-- Order by the first field ascending.  When first fields are equal
\-- order by second field descending.
example :: 'S.Select' ('Opaleye.Field.Field' 'T.SqlInt4', 'Opaleye.Field.Field' 'T.SqlText')
        -> 'S.Select' ('Opaleye.Field.Field' 'T.SqlInt4', 'Opaleye.Field.Field' 'T.SqlText')
example = 'orderBy' ('asc' fst \<\> 'desc' snd)
@

-}
orderBy :: O.Order a -> S.Select a -> S.Select a
orderBy os q =
  Q.productQueryArr (O.orderByU os . Q.runSimpleQueryArr q)

-- | Specify an ascending ordering by the given expression.
--   (Any NULLs appear last)
asc :: SqlOrd b => (a -> F.Field b) -> O.Order a
asc = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpAsc
                          , HPQ.orderNulls     = HPQ.NullsLast }

-- | Specify an descending ordering by the given expression.
--   (Any NULLs appear first)
desc :: SqlOrd b => (a -> F.Field b) -> O.Order a
desc = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpDesc
                           , HPQ.orderNulls     = HPQ.NullsFirst }

-- | Specify an ascending ordering by the given expression.
--   (Any NULLs appear first)
ascNullsFirst :: SqlOrd b => (a -> F.Field b) -> O.Order a
ascNullsFirst = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpAsc
                                    , HPQ.orderNulls     = HPQ.NullsFirst }


-- | Specify an descending ordering by the given expression.
--   (Any NULLs appear last)
descNullsLast :: SqlOrd b => (a -> F.Field b) -> O.Order a
descNullsLast = O.order HPQ.OrderOp { HPQ.orderDirection = HPQ.OpDesc
                                    , HPQ.orderNulls     = HPQ.NullsLast }

-- * Limit and offset

{- |
Limit the results of the given 'S.Select' to the given maximum number of
items.

/WARNING:/ If you're planning on using limit/offset together please use
'offset' /before/ you use 'limit', e.g.:

@
limit 10 (offset 50 yourSelect)
@

This is because Opaleye applies @OFFSET@ and @LIMIT@ to the @SELECT@ separately.
The result of the 'S.Select' given above is the following, which will return
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
limit :: Int -> S.Select a -> S.Select a
limit n a = Q.productQueryArr (O.limit' n . Q.runSimpleQueryArr a)

{- |
Offset the results of the given 'S.Select' by the given amount, skipping
that many result rows.

/WARNING:/ Please read the documentation of 'limit' before combining
'offset' with 'limit'.
-}
offset :: Int -> S.Select a -> S.Select a
offset n a = Q.productQueryArr (O.offset' n . Q.runSimpleQueryArr a)

-- * Distinct on

-- | Use 'distinctOn' instead.  Will be deprecated in 0.9.
distinctOnCorrect :: D.Default U.Unpackspec b b
                  => (a -> b)
                  -> S.Select a
                  -> S.Select a
distinctOnCorrect proj q = Q.productQueryArr (O.distinctOn D.def proj . Q.runSimpleQueryArr q)

-- | Use 'distinctOnBy' instead.  Will be deprecated in 0.9.
distinctOnByCorrect :: D.Default U.Unpackspec b b
                    => (a -> b)
                    -> O.Order a
                    -> S.Select a
                    -> S.Select a
distinctOnByCorrect proj ord q = Q.productQueryArr (O.distinctOnBy D.def proj ord . Q.runSimpleQueryArr q)


-- * Other

-- | Typeclass for Postgres types which support ordering operations.
class SqlOrd a where

instance SqlOrd T.SqlBool
instance SqlOrd T.SqlDate
instance SqlOrd T.SqlFloat8
instance SqlOrd T.SqlFloat4
instance SqlOrd T.SqlInt8
instance SqlOrd T.SqlInt4
instance SqlOrd T.SqlInt2
instance SqlOrd T.SqlNumeric
instance SqlOrd T.SqlText
instance SqlOrd T.SqlVarcharN
instance SqlOrd T.SqlTime
instance SqlOrd T.SqlTimestamptz
instance SqlOrd T.SqlTimestamp
instance SqlOrd T.SqlCitext
instance SqlOrd T.SqlUuid

-- | Keep a row from each set where the given function returns the same result. No
--   ordering is guaranteed. Multiple fields may be distinguished by projecting out
--   tuples of 'Opaleye.Field.Field_'s. Use 'distinctOnBy' to control how the rows
--   are chosen.
distinctOn :: D.Default U.Unpackspec b b => (a -> b) -> S.Select a -> S.Select a
distinctOn = distinctOnCorrect

-- | Keep the row from each set where the given function returns the same result. The
--   row is chosen according to which comes first by the supplied ordering. However, no
--   output ordering is guaranteed. Mutliple fields may be distinguished by projecting
--   out tuples of 'Opaleye.Field.Field_'s.
distinctOnBy :: D.Default U.Unpackspec b b => (a -> b) -> O.Order a
             -> S.Select a -> S.Select a
distinctOnBy = distinctOnByCorrect
