{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Distinct where

import qualified Opaleye.QueryArr as Q
import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Database.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

import qualified Control.Monad.Trans.State as S

distinct :: D.Default U.Unpackspec columns columns =>
            Query columns -> Query columns
distinct = distinctExplicit D.def

distinctExplicit :: U.Unpackspec columns columns'
                 -> Query columns -> Query columns'
distinctExplicit unpack q = Q.simpleQueryArr (distinctU unpack
                                              . Q.runSimpleQueryArr q)

-- TODO: duplication with runQueryArrUnpack
-- TODO: move this to Internal
distinctU :: U.Unpackspec columns columns'
          -> (columns, PQ.PrimQuery, T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
distinctU unpack (columns, primQ, t) = (newColumns, primQ', t)
  where -- TODO: This should really be a writer for the writable part
        f :: HPQ.PrimExpr
          -> S.State ([(String, Maybe HPQ.AggrOp, HPQ.PrimExpr)], Int)
                     HPQ.PrimExpr
        f pe = do
          (groupPEs, i) <- S.get
          let s = "result" ++ show i
          S.put (groupPEs ++ [(s, Nothing, pe)], i+1)
          return (HPQ.AttrExpr s)

        (newColumns, (groupPEs', _)) =
          S.runState (U.runUnpackspec unpack f columns) ([], 0)

        primQ' = PQ.Aggregate groupPEs' primQ
