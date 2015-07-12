{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Values where

import qualified Opaleye.PGTypes as T

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Data.Profunctor (Profunctor, dimap, rmap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

-- There are two annoyances with creating SQL VALUES statements
--
-- 1. SQL does not allow empty VALUES statements so if we want to
--    create a VALUES statement from an empty list we have to fake it
--    somehow.  The current approach is to make a VALUES statement
--    with a single row of NULLs and then restrict it with WHERE
--    FALSE.

-- 2. Postgres's type inference of constants is pretty poor so we will
--    sometimes have to give explicit type signatures.  The future
--    ShowConstant class will have the same problem.  NB We don't
--    actually currently address this problem.

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
                   -> PM.PM [(HPQ.Symbol, HPQ.PrimExpr)] HPQ.PrimExpr
extractValuesField = PM.extractAttr "values"

newtype Valuesspec columns columns' =
  Valuesspec (PM.PackMap HPQ.PrimExpr HPQ.PrimExpr () columns')

runValuesspec :: Applicative f => Valuesspec columns columns'
              -> (HPQ.PrimExpr -> f HPQ.PrimExpr) -> f columns'
runValuesspec (Valuesspec v) f = PM.traversePM v f ()

instance Default Valuesspec (Column T.PGInt4) (Column T.PGInt4) where
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
