{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Values where

import qualified Opaleye.QueryArr as Q
import           Opaleye.QueryArr (Query)

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Database.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

values :: D.Default U.Unpackspec columns columns =>
          [columns] -> Q.Query columns
values = valuesExplicit D.def

valuesExplicit :: U.Unpackspec columns columns'
               -> [columns] -> Query columns'
valuesExplicit unpack columns = Q.simpleQueryArr (valuesU unpack columns)

valuesU :: U.Unpackspec columns columns'
        -> [columns]
        -> ((), T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
valuesU unpack rows ((), t) = (newColumns, primQ', t)
  where runRow row = valuesRow
           where (_, valuesRow) =
                   PM.run (U.runUnpackspec unpack extractValuesEntry row)

        values' :: [[HPQ.PrimExpr]]
        values' = map runRow rows

        (newColumns, valuesPEs) =
          -- FIXME: danger!
          PM.run (U.runUnpackspec unpack extractValuesField (rows !! 0))

        primQ' = PQ.Values valuesPEs values'


-- We don't actually use the return value of this.  It might be better
-- to come up with another Applicative instance for specifically doing
-- what we need.
extractValuesEntry :: HPQ.PrimExpr -> PM.PM [HPQ.PrimExpr] HPQ.PrimExpr
extractValuesEntry pe = do
  PM.write pe
  return pe

-- FIXME: need to use a tag
extractValuesField :: HPQ.PrimExpr -> PM.PM [String] HPQ.PrimExpr
extractValuesField _ = do
  i <- PM.new
  let s = "values" ++ i
  PM.write s
  return (HPQ.AttrExpr s)
