-- | A 'Select' represents an SQL @SELECT@ statment.  To run a
-- 'Select' use the functions in "Opaleye.RunSelect".  To create a
-- 'Select' you probably want to start by querying one of your
-- 'Opaleye.Table.Table's using 'Opaleye.Table.selectTable'.
-- 'SelectArr' is a parametrised version of 'Select', i.e. it can be
-- passed arguments.

module Opaleye.Select (Select, SelectArr) where

import Opaleye.Internal.QueryArr
