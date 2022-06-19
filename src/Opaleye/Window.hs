module Opaleye.Window
       (
         -- * Window queries
         W.window
       , W.Window
       , W.over
       , W.Partition
       , W.partitionBy
       , W.orderPartitionBy

         -- ** Specific window functions
       , W.cumulative
       , rowNumber
       , rank
       , denseRank
       , percentRank
       , cumeDist
       , ntile
       , lag
       , lead
       , firstValue
       , lastValue
       , nthValue
       ) where

import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.Window as W

import qualified Opaleye.Field as F
import qualified Opaleye.SqlTypes as T

-- This page of Postgres documentation tell us what window
-- functions are available
--
--   https://www.postgresql.org/docs/devel/functions-window.html


-- | [@row_number()@](https://www.postgresql.org/docs/current/functions-window.html)
rowNumber :: W.Window (F.Field T.SqlInt8)
rowNumber = W.makeWndw HPQ.WndwRowNumber


-- | [@rank()@](https://www.postgresql.org/docs/current/functions-window.html)
rank :: W.Window (F.Field T.SqlInt8)
rank = W.makeWndw HPQ.WndwRank


-- | [@dense_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
denseRank :: W.Window (F.Field T.SqlInt8)
denseRank = W.makeWndw HPQ.WndwDenseRank


-- | [@percent_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
percentRank :: W.Window (F.Field T.SqlFloat8)
percentRank = W.makeWndw HPQ.WndwPercentRank


-- | [@cume_dist()@](https://www.postgresql.org/docs/current/functions-window.html)
cumeDist :: W.Window (F.Field T.SqlFloat8)
cumeDist = W.makeWndw HPQ.WndwCumeDist


-- | [@ntile(num_buckets)@](https://www.postgresql.org/docs/current/functions-window.html)
ntile :: F.Field T.SqlInt4 -> W.Window (F.Field T.SqlInt4)
ntile (IC.Column buckets) = W.makeWndw $ HPQ.WndwNtile buckets


-- | [@lag(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lag :: F.Field_ n a -> F.Field T.SqlInt4 -> F.Field_ n a -> W.Window (F.Field_ n a)
lag (IC.Column a) (IC.Column offset) (IC.Column def) =
  W.makeWndw $ HPQ.WndwLag a offset def


-- | [@lead(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lead :: F.Field_ n a -> F.Field T.SqlInt4 -> F.Field_ n a -> W.Window (F.Field_ n a)
lead (IC.Column a) (IC.Column offset) (IC.Column def) =
  W.makeWndw $ HPQ.WndwLead a offset def


-- | [@first_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
firstValue :: F.Field_ n a -> W.Window (F.Field_ n a)
firstValue (IC.Column a) = W.makeWndw $ HPQ.WndwFirstValue a


-- | [@last_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
lastValue :: F.Field_ n a -> W.Window (F.Field_ n a)
lastValue (IC.Column a) = W.makeWndw $ HPQ.WndwLastValue a


-- | [@nth_value(value, n)@](https://www.postgresql.org/docs/current/functions-window.html)
nthValue :: F.Field_ n a -> F.Field T.SqlInt4 -> W.Window (F.FieldNullable a)
nthValue (IC.Column a) (IC.Column n) = W.makeWndw $ HPQ.WndwNthValue a n
