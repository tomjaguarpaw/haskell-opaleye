-- | 'Select' and 'SelectArr' are the composable units of database
-- querying that are used in Opaleye.

module Opaleye.Select where

import qualified Opaleye.QueryArr as Q

-- | A @SELECT@, i.e. some functionality that can run via SQL
-- and produce a collection of rows.
--
-- @Select a@ is analogous to a Haskell value @[a]@.
type Select = SelectArr ()

-- | @SelectArr a b@ is analogous to a Haskell function @a -> [b]@.
type SelectArr = Q.QueryArr
