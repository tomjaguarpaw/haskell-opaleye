module Opaleye.MaybeFields (
  MaybeFields,
  nothingFields,
  justFields,
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
  ) where

import Opaleye.Internal.MaybeFields
import Opaleye.Internal.Values
