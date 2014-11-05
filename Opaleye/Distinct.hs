{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Distinct where

import qualified Opaleye.QueryArr as Q
import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.Distinct as D
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
distinctExplicit unpack q = Q.simpleQueryArr (D.distinctU unpack
                                              . Q.runSimpleQueryArr q)
