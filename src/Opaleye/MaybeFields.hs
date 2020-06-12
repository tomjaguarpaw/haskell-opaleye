{-# LANGUAGE FlexibleContexts #-}

-- | 'MaybeFields' is Opaleye's analogue to 'Data.Maybe.Maybe'.  You
-- probably won't want to create values of type 'MaybeFields'
-- directly; instead they will appear as the result of
-- left\/right\/outer join-like operations, such as
-- 'Opaleye.Join.optionalRestrict' and 'optional'.

module Opaleye.MaybeFields (
  MaybeFields,
  maybeFields,
  fromMaybeFields,
  catMaybeFields,
  Opaleye.MaybeFields.traverseMaybeFields,
  -- * Explicit versions
  maybeFieldsExplicit,
  fromMaybeFieldsExplicit,
  traverseMaybeFieldsExplicit,
  -- * Adaptors
  fromFieldsMaybeFields,
  unpackspecMaybeFields
                           ) where

import Opaleye.Internal.MaybeFields
import Opaleye.Unpackspec
import Opaleye.Select

import Data.Profunctor.Product.Default

-- | 'traverseMaybeFields' is analogous to Haskell's
-- @'Data.Traversable.traverse' :: (a -> [b]) -> 'Data.Maybe.Maybe' a
-- -> ['Data.Maybe.Maybe' b]@.  In particular,
-- 'Data.Traversable.traverse' satisfies the following properties that
-- generalise to 'traverseMaybeFields':
--
--   * @traverse f Nothing = [Nothing]@
--   * @traverse (const l)  (Just x) = fmap Just l@
--   * @traverse (const []) (Just x) = []@
traverseMaybeFields :: (Default Unpackspec a a, Default Unpackspec b b)
                    => SelectArr a b
                    -- ^
                    -> SelectArr (MaybeFields a) (MaybeFields b)
                    -- ^
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
