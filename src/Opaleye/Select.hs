-- | A 'Select' represents an SQL @SELECT@ statment.  To run a
-- 'Select' use the functions in "Opaleye.RunSelect".  To create a
-- 'Select' you probably want to start by querying one of your
-- 'Opaleye.Table.Table's using 'Opaleye.Table.selectTable'.
-- 'SelectArr' is a parametrised version of 'Select', i.e. it can be
-- passed arguments.

module Opaleye.Select where

import qualified Opaleye.QueryArr as Q

-- | A @SELECT@, i.e. an SQL query which produces a collection of
-- rows.
--
-- @Select a@ is analogous to a Haskell value @[a]@.
type Select = SelectArr ()

-- | A parametrised 'Select'.  A @SelectArr a b@ accepts an argument
-- of type @a@.
--
-- @SelectArr a b@ is analogous to a Haskell function @a -> [b]@.
type SelectArr = Q.QueryArr
