module Opaleye.Test.Algebra where

import qualified Test.QuickCheck as TQ
import           Test.QuickCheck ((===), (.&&.))

import           Data.Semigroup

associative :: (Eq a, Show a, Semigroup a) => a -> a -> a -> TQ.Property
associative x1 x2 x3 = (x1 <> (x2 <> x3)) === ((x1 <> x2) <> x3)

identity :: (Eq a, Show a, Semigroup a, Monoid a) => a -> TQ.Property
identity x = ((x <> mempty) === x)
             .&&.
             ((mempty <> x) === x)
