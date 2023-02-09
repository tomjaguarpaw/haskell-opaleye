-- {-# OPTIONS_HADDOCK ignore-exports #-}

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

module Opaleye ( module Opaleye.Adaptors
               , module Opaleye.Aggregate
               , module Opaleye.Binary
               , module Opaleye.Column
               , module Opaleye.Distinct
               , module Opaleye.Field
               , module Opaleye.FunctionalJoin
               , module Opaleye.Join
               , module Opaleye.Label
               , module Opaleye.Lateral
               , module Opaleye.Manipulation
               , module Opaleye.MaybeFields
               , module Opaleye.Operators
               , module Opaleye.Order
               , module Opaleye.RunSelect
               , module Opaleye.Sql
               , module Opaleye.Select
               , module Opaleye.SqlTypes
               , module Opaleye.Table
               , module Opaleye.ToFields
               , module Opaleye.Values
               , module Opaleye.With
               , module Opaleye.Window
               ) where

import Opaleye.Adaptors
import Opaleye.Aggregate
import Opaleye.Binary
import Opaleye.Column
  hiding (null,
          isNull)
import Opaleye.Distinct
import Opaleye.Field
import Opaleye.FunctionalJoin
import Opaleye.Join
import Opaleye.Label
import Opaleye.Lateral
import Opaleye.Manipulation
import Opaleye.MaybeFields
import Opaleye.Operators
import Opaleye.Order
import Opaleye.RunSelect
import Opaleye.Select
import Opaleye.Sql
import Opaleye.SqlTypes
import Opaleye.Table
import Opaleye.ToFields
import Opaleye.Values
import Opaleye.Window
import Opaleye.With
