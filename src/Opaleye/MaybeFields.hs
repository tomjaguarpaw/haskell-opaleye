module Opaleye.MaybeFields (
  MaybeFields,
  nothingFields,
  justFields,
  fromMaybeFields,
  maybeFields,
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
  ) where

import Opaleye.Internal.MaybeFields
import Opaleye.Internal.Values
