{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opaleye.Internal.Distinct where

import qualified Opaleye.Internal.MaybeFields as M
import           Opaleye.Select (Select)
import           Opaleye.Field (Field_)
import           Opaleye.Aggregate (Aggregator, groupBy, aggregate)

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

-- We implement distinct simply by grouping by all columns.  We could
-- instead implement it as SQL's DISTINCT but implementing it in terms
-- of something else that we already have is easier at this point.

distinctExplicit :: Distinctspec fields fields'
                 -> Select fields -> Select fields'
distinctExplicit (Distinctspec agg) = aggregate agg

newtype Distinctspec a b = Distinctspec (Aggregator a b)

instance Default Distinctspec (Field_ n a) (Field_ n a) where
  def = Distinctspec groupBy

distinctspecField :: Distinctspec (Field_ n a) (Field_ n a)
distinctspecField = def

distinctspecMaybeFields :: M.WithNulls Distinctspec a b
                        -> Distinctspec (M.MaybeFields a) (M.MaybeFields b)
distinctspecMaybeFields = M.unWithNulls def

instance Default (M.WithNulls Distinctspec) a b
  => Default Distinctspec (M.MaybeFields a) (M.MaybeFields b) where
  def = distinctspecMaybeFields def

-- { Boilerplate instances

instance Functor (Distinctspec a) where
  fmap f (Distinctspec g) = Distinctspec (fmap f g)

instance Applicative (Distinctspec a) where
  pure = Distinctspec . pure
  Distinctspec f <*> Distinctspec x = Distinctspec (f <*> x)

instance P.Profunctor Distinctspec where
  dimap f g (Distinctspec q) = Distinctspec (P.dimap f g q)

instance PP.ProductProfunctor Distinctspec where
  purePP = pure
  (****) = (<*>)

instance PP.SumProfunctor Distinctspec where
  Distinctspec x1 +++! Distinctspec x2 = Distinctspec (x1 PP.+++! x2)

-- }
