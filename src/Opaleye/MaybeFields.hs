-- | 'MaybeFields' is Opaleye's analogue to 'Data.Maybe.Maybe'.  You
-- probably won't want to create values of type 'MaybeFields'
-- directly; instead they will appear as the result of
-- left\/right\/outer join-like operations, such as
-- 'Opaleye.Join.optionalRestrict' and 'Opaleye.Join.optional'.

module Opaleye.MaybeFields (
  -- * 'MaybeFields' type
  MaybeFields,
  -- * Creating a 'MaybeFields'
  nothingFields,
  nothingFieldsOfTypeOf,
  justFields,
  nullableToMaybeFields,
  -- * Using a 'MaybeFields'
  matchMaybe,
  fromMaybeFields,
  maybeFields,
  maybeFieldsToNullable,
  isJustAnd,
  -- * Creating a 'Select' which returns 'MaybeFields'
  Opaleye.Join.optional,
  Opaleye.MaybeFields.traverseMaybeFields,
  -- * Using a 'Select' which returns 'MaybeFields'
  catMaybeFields,
  maybeFieldsToSelect,
  -- * Adaptors
  Nullspec,
  nullspecField,
  nullspecMaybeFields,
  nullspecList,
  nullspecEitherLeft,
  nullspecEitherRight,
  binaryspecMaybeFields,
  distinctspecMaybeFields,
  fromFieldsMaybeFields,
  toFieldsMaybeFields,
  unpackspecMaybeFields,
  valuesspecMaybeFields,
  -- * Explicit versions
  nothingFieldsExplicit,
  fromMaybeFieldsExplicit,
  maybeFieldsExplicit,
  Opaleye.Join.optionalExplicit,
  traverseMaybeFieldsExplicit,
  ) where

import Opaleye.Internal.Distinct
import Opaleye.Internal.MaybeFields
import Opaleye.Internal.Values
import Opaleye.Join
import Opaleye.Internal.Unpackspec
import Opaleye.Select

import Data.Profunctor.Product.Default

-- | 'traverseMaybeFields' is analogous to Haskell's
-- @'Data.Traversable.traverse' :: (a -> [b]) -> 'Data.Maybe.Maybe' a
-- -> ['Data.Maybe.Maybe' b]@.  In particular,
-- 'Data.Traversable.traverse' has the following definition that
-- generalises to 'traverseMaybeFields':
--
-- * @traverse _ Nothing = pure Nothing@
-- * @traverse f (Just x) = fmap Just (f x)@
traverseMaybeFields :: (Default Unpackspec a a, Default Unpackspec b b)
                    => SelectArr a b
                    -- ^
                    -> SelectArr (MaybeFields a) (MaybeFields b)
                    -- ^ ͘
traverseMaybeFields = Opaleye.Internal.MaybeFields.traverseMaybeFields

-- The Unpackspecs are currently redundant, but I'm adding them in
-- case they become necessary in the future.  Then we can use them
-- without breaking the API.
traverseMaybeFieldsExplicit :: Unpackspec a a
                            -> Unpackspec b b
                            -> SelectArr a b
                            -> SelectArr (MaybeFields a) (MaybeFields b)
traverseMaybeFieldsExplicit _ _ =
  Opaleye.Internal.MaybeFields.traverseMaybeFields
