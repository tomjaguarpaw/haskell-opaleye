{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Values where

import           Opaleye.Column (Column(Column))

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Database.HaskellDB.PrimQuery as HPQ

import           Data.Profunctor (Profunctor, dimap, rmap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

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
        nulls = map snd valuesPEs_nulls

        yieldNoRows :: PQ.PrimQuery -> PQ.PrimQuery
        yieldNoRows = PQ.restrict (HPQ.ConstExpr (HPQ.BoolLit False))

        values' :: [[HPQ.PrimExpr]]
        (values', wrap) = if null rows
                          then ([nulls], yieldNoRows)
                          else (map runRow rows, id)

        primQ' = wrap (PQ.Values valuesPEs values')

-- We don't actually use the return value of this.  It might be better
-- to come up with another Applicative instance for specifically doing
-- what we need.
extractValuesEntry :: HPQ.PrimExpr -> PM.PM [HPQ.PrimExpr] HPQ.PrimExpr
extractValuesEntry pe = do
  PM.write pe
  return pe

extractValuesField :: T.Tag -> HPQ.PrimExpr
                   -> PM.PM [(String, HPQ.PrimExpr)] HPQ.PrimExpr
extractValuesField t theNull = do
  i <- PM.new
  let s = T.tagWith t ("values" ++ i)
  PM.write (s, theNull)
  return (HPQ.AttrExpr s)

data Valuesspec columns columns' =
  Valuesspec (PM.PackMap HPQ.PrimExpr HPQ.PrimExpr () columns')

runValuesspec :: Applicative f => Valuesspec columns columns'
              -> (HPQ.PrimExpr -> f HPQ.PrimExpr) -> f columns'
runValuesspec (Valuesspec v) f = PM.packmap v f ()

instance Default Valuesspec (Column Int) (Column Int) where
  def = Valuesspec (PM.PackMap (\f () -> fmap Column (f (HPQ.ConstExpr HPQ.NullLit))))

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (Valuesspec a) where
  fmap f (Valuesspec g) = Valuesspec (fmap f g)

instance Applicative (Valuesspec a) where
  pure = Valuesspec . pure
  Valuesspec f <*> Valuesspec x = Valuesspec (f <*> x)

instance Profunctor Valuesspec where
  dimap _ g (Valuesspec q) = Valuesspec (rmap g q)

instance ProductProfunctor Valuesspec where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

-- }
