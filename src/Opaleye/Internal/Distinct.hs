{-# LANGUAGE MultiParamTypeClasses #-}

module Opaleye.Internal.Distinct where

import           Opaleye.QueryArr (Query)
import           Opaleye.Column (Column)
import           Opaleye.Aggregate (Aggregator, groupBy, aggregate)

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

-- We implement distinct simply by grouping by all columns.  We could
-- instead implement it as SQL's DISTINCT but implementing it in terms
-- of something else that we already have is easier at this point.

distinctExplicit :: Distinctspec columns columns'
                 -> Query columns -> Query columns'
distinctExplicit (Distinctspec agg) = aggregate agg

newtype Distinctspec a b = Distinctspec (Aggregator a b)

instance Default Distinctspec (Column a) (Column a) where
  def = Distinctspec groupBy

-- { Boilerplate instances

instance Functor (Distinctspec a) where
  fmap f (Distinctspec g) = Distinctspec (fmap f g)

instance Applicative (Distinctspec a) where
  pure = Distinctspec . pure
  Distinctspec f <*> Distinctspec x = Distinctspec (f <*> x)

instance P.Profunctor Distinctspec where
  dimap f g (Distinctspec q) = Distinctspec (P.dimap f g q)

instance PP.ProductProfunctor Distinctspec where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor Distinctspec where
  Distinctspec x1 +++! Distinctspec x2 = Distinctspec (x1 PP.+++! x2)

-- }
