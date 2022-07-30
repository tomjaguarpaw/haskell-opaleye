-- | Support for [PostgreSQL window
-- functions](https://www.postgresql.org/docs/current/tutorial-window.html)

module Opaleye.Window
       (
         -- * Run window functions on a @Select@
         W.runWindows

         -- * Create @Windows@
       , W.Windows
       , W.over

         -- * Create a @Window@
       , W.Window
       , W.partitionBy

         -- * Create a @WindowFunction@
       , W.WindowFunction

         -- * Window functions

         -- | You might like to also refer to [the Postgres
         -- documentation page that describes its window
         -- functions](https://www.postgresql.org/docs/devel/functions-window.html).

       , W.noWindowFunction
       , W.aggregatorWindowFunction
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

-- | [@row_number()@](https://www.postgresql.org/docs/current/functions-window.html)
rowNumber :: W.WindowFunction a (F.Field T.SqlInt8)
rowNumber = W.makeWndwAny HPQ.WndwRowNumber


-- | [@rank()@](https://www.postgresql.org/docs/current/functions-window.html)
rank :: W.WindowFunction a (F.Field T.SqlInt8)
rank = W.makeWndwAny HPQ.WndwRank


-- | [@dense_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
denseRank :: W.WindowFunction a (F.Field T.SqlInt8)
denseRank = W.makeWndwAny HPQ.WndwDenseRank


-- | [@percent_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
percentRank :: W.WindowFunction a (F.Field T.SqlFloat8)
percentRank = W.makeWndwAny HPQ.WndwPercentRank


-- | [@cume_dist()@](https://www.postgresql.org/docs/current/functions-window.html)
cumeDist :: W.WindowFunction a (F.Field T.SqlFloat8)
cumeDist = W.makeWndwAny HPQ.WndwCumeDist


-- | [@ntile(num_buckets)@](https://www.postgresql.org/docs/current/functions-window.html)
ntile :: F.Field T.SqlInt4
      -- ^ num_buckets
      -> W.WindowFunction a (F.Field T.SqlInt4)
ntile (IC.Column buckets) = W.makeWndwAny $ HPQ.WndwNtile buckets


-- | [@lag(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lag :: F.Field T.SqlInt4
    -- ^ offset
    -> F.Field_ n a
    -- ^ default
    -> W.WindowFunction (F.Field_ n a) (F.Field_ n a)
lag (IC.Column offset) (IC.Column def) =
  W.makeWndwField $ \a -> HPQ.WndwLag a offset def


-- | [@lead(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lead :: F.Field T.SqlInt4
     -- ^ offset
     -> F.Field_ n a
     -- ^ default
     -> W.WindowFunction (F.Field_ n a) (F.Field_ n a)
lead (IC.Column offset) (IC.Column def) =
  W.makeWndwField $ \a -> HPQ.WndwLead a offset def


-- | [@first_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
firstValue :: W.WindowFunction (F.Field_ n a) (F.Field_ n a)
firstValue = W.makeWndwField HPQ.WndwFirstValue


-- | [@last_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
lastValue :: W.WindowFunction (F.Field_ n a) (F.Field_ n a)
lastValue = W.makeWndwField HPQ.WndwLastValue


-- | [@nth_value(value, n)@](https://www.postgresql.org/docs/current/functions-window.html)
nthValue :: F.Field T.SqlInt4
         -- ^ n
         -> W.WindowFunction (F.Field_ n a) (F.FieldNullable a)
nthValue (IC.Column n) = W.makeWndwField $ \a -> HPQ.WndwNthValue a n
