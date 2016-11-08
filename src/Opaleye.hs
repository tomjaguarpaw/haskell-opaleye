-- | An SQL-generating DSL targeting PostgreSQL.  Allows Postgres
--   queries to be written within Haskell in a typesafe and composable
--   fashion.
--
-- You might like to look at
--
-- * <https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs Basic tutorial>
--
-- * <https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialManipulation.lhs Manipulation tutorial>
--
-- * <https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialAdvanced.lhs Advanced tutorial>
--
-- * If you are confused about the @Default@ typeclass, then
-- the <https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/DefaultExplanation.lhs Default explanation>

module Opaleye ( module Opaleye.Aggregate
               , module Opaleye.Binary
               , module Opaleye.Column
               , module Opaleye.Constant
               , module Opaleye.Distinct
               , module Opaleye.FunctionalJoin
               , module Opaleye.Join
               , module Opaleye.Label
               , module Opaleye.Manipulation
               , module Opaleye.Operators
               , module Opaleye.Order
               , module Opaleye.PGTypes
               , module Opaleye.QueryArr
               , module Opaleye.RunQuery
               , module Opaleye.Sql
               , module Opaleye.Table
               , module Opaleye.Values
               ) where

import Opaleye.Aggregate
import Opaleye.Binary
import Opaleye.Column
import Opaleye.Constant
import Opaleye.Distinct
import Opaleye.FunctionalJoin
import Opaleye.Join
import Opaleye.Label
import Opaleye.Manipulation
import Opaleye.Operators
import Opaleye.Order
import Opaleye.PGTypes
import Opaleye.QueryArr
import Opaleye.RunQuery
import Opaleye.Sql
import Opaleye.Table
import Opaleye.Values
