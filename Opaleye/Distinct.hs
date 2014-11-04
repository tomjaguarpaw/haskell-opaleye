{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Distinct where

import qualified Opaleye.QueryArr as Q
import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Database.HaskellDB.PrimQuery as PQ

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
distinctU :: U.Unpackspec columns columns'
          -> (columns, PQ.PrimQuery, T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
distinctU unpack (columns, primQ, t) = (newColumns, primQ', t)
  where -- TODO: This should really be a writer for the writable part
        f :: PQ.PrimExpr -> S.State ([(String, PQ.PrimExpr)], Int) PQ.PrimExpr
        f pe = do
          (groupPEs, i) <- S.get
          let s = "result" ++ show i
          S.put (groupPEs ++ [(s, pe)], i+1)
          return (PQ.AttrExpr s)

        (newColumns, (groupPEs', _)) =
          S.runState (U.runUnpackspec unpack f columns) ([], 0)

        primQ' = PQ.Group groupPEs' primQ
