{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Opaleye.Internal.Values where

import           Opaleye.Internal.Column (Column)
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.List.NonEmpty as NEL
import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

-- FIXME: We don't currently handle the case of zero columns.  Need to
-- emit a dummy column and data.
valuesU :: U.Unpackspec columns columns'
        -> Valuesspec columns columns'
        -> [columns]
        -> ((), T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
valuesU unpack valuesspec rows ((), t) = (newColumns, primQ', T.next t)
  where runRow row = valuesRow
           where (_, valuesRow) =
                   PM.run (U.runUnpackspec unpack extractValuesEntry row)

        (newColumns, valuesPEs_nulls) =
          PM.run (runValuesspec valuesspec (extractValuesField t))

        valuesPEs = map fst valuesPEs_nulls

        values :: [[HPQ.PrimExpr]]
        values = map runRow rows

        primQ' = case NEL.nonEmpty values of
          Nothing      -> PQ.Empty ()
          Just values' -> PQ.Values valuesPEs values'

-- We don't actually use the return value of this.  It might be better
-- to come up with another Applicative instance for specifically doing
-- what we need.
extractValuesEntry :: HPQ.PrimExpr -> PM.PM [HPQ.PrimExpr] HPQ.PrimExpr
extractValuesEntry pe = do
  PM.write pe
  return pe

extractValuesField :: T.Tag -> primExpr
                   -> PM.PM [(HPQ.Symbol, primExpr)] HPQ.PrimExpr
extractValuesField = PM.extractAttr "values"

data Unit a = Unit deriving Functor

newtype Valuesspec columns columns' =
  Valuesspec (PM.PackMapColumn Unit columns columns')

runValuesspec :: Applicative f => Valuesspec columns columns'
              -> (() -> f HPQ.PrimExpr) -> f columns'
runValuesspec (Valuesspec (PM.PackMapColumn v)) f =
  (PM.traversePM v (f . const ())
  . const Unit)
  ()

instance Default Valuesspec (Column a) (Column a) where
  def = Valuesspec PM.pmColumn

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (Valuesspec a) where
  fmap f (Valuesspec g) = Valuesspec (fmap f g)

instance Applicative (Valuesspec a) where
  pure = Valuesspec . pure
  Valuesspec f <*> Valuesspec x = Valuesspec (f <*> x)

instance Profunctor Valuesspec where
  dimap f g (Valuesspec q) = Valuesspec (dimap f g q)

instance ProductProfunctor Valuesspec where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

-- }
