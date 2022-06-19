module Opaleye.Window
       (
         -- * Window queries
         W.window
       , W.Window
       , W.over
       , W.Partition
       , W.partitionBy
       , W.orderBy

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


rowNumber :: W.Window (F.Field T.SqlInt8)
rowNumber = W.makeWndw HPQ.WndwRowNumber


rank :: W.Window (F.Field T.SqlInt8)
rank = W.makeWndw HPQ.WndwRank


denseRank :: W.Window (F.Field T.SqlInt8)
denseRank = W.makeWndw HPQ.WndwDenseRank


percentRank :: W.Window (F.Field T.SqlFloat8)
percentRank = W.makeWndw HPQ.WndwPercentRank


cumeDist :: W.Window (F.Field T.SqlFloat8)
cumeDist = W.makeWndw HPQ.WndwCumeDist


ntile :: F.Field T.SqlInt4 -> W.Window (F.Field T.SqlInt4)
ntile (IC.Column buckets) = W.makeWndw $ HPQ.WndwNtile buckets


lag :: F.Field_ n a -> F.Field T.SqlInt4 -> F.Field_ n a -> W.Window (F.Field_ n a)
lag (IC.Column a) (IC.Column offset) (IC.Column def) =
  W.makeWndw $ HPQ.WndwLag a offset def


lead :: F.Field_ n a -> F.Field T.SqlInt4 -> F.Field_ n a -> W.Window (F.Field_ n a)
lead (IC.Column a) (IC.Column offset) (IC.Column def) =
  W.makeWndw $ HPQ.WndwLead a offset def


firstValue :: F.Field_ n a -> W.Window (F.Field_ n a)
firstValue (IC.Column a) = W.makeWndw $ HPQ.WndwFirstValue a


lastValue :: F.Field_ n a -> W.Window (F.Field_ n a)
lastValue (IC.Column a) = W.makeWndw $ HPQ.WndwLastValue a


nthValue :: F.Field_ n a -> F.Field T.SqlInt4 -> W.Window (F.FieldNullable a)
nthValue (IC.Column a) (IC.Column n) = W.makeWndw $ HPQ.WndwNthValue a n
