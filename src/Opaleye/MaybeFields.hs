{-# LANGUAGE FlexibleContexts #-}

module Opaleye.MaybeFields (
  MaybeFields,
  nothingFields,
  justFields,
  fromMaybeFields,
  maybeFields,
  maybeFieldsToSelect,
  optional,
  Opaleye.MaybeFields.traverseMaybeFields,
  -- * Adaptors
  Nullspec,
  nullspecField,
  nullspecMaybeFields,
  nullspecList,
  nullspecEitherLeft,
  nullspecEitherRight,
  fromFieldsMaybeFields,
  toFieldsMaybeFields,
  unpackspecMaybeFields,
  valuesspecMaybeFields,
  -- * Explicit versions
  nothingFieldsExplicit,
  fromMaybeFieldsExplicit,
  maybeFieldsExplicit,
  traverseMaybeFieldsExplicit,
  ) where

import Opaleye.Internal.MaybeFields
import Opaleye.Internal.Values
import Opaleye.Unpackspec
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
