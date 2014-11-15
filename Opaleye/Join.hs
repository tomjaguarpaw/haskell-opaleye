{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Join where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Join as J
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import           Opaleye.Column (Column(Column))

import qualified Data.Profunctor.Product.Default as D

leftJoin  :: (D.Default U.Unpackspec columnsA columnsA,
              D.Default U.Unpackspec columnsB columnsB,
              D.Default J.NullMaker columnsB nullableColumnsB) =>
             Query columnsA -> Query columnsB
          -> ((columnsA, columnsB) -> Column Bool)
          -> Query (columnsA, nullableColumnsB)
leftJoin = leftJoinExplicit D.def D.def D.def

leftJoinExplicit :: U.Unpackspec columnsA columnsA
                 -> U.Unpackspec columnsB columnsB
                 -> J.NullMaker columnsB nullableColumnsB
                 -> Query columnsA -> Query columnsB
                 -> ((columnsA, columnsB) -> Column Bool)
                 -> Query (columnsA, nullableColumnsB)
leftJoinExplicit unpackA unpackB nullmaker qA qB cond = Q.simpleQueryArr q where
  q ((), startTag) = ((newColumnsA, nullableColumnsB), primQueryR, T.next endTag)
    where (columnsA, primQueryA, midTag) = Q.runSimpleQueryArr qA ((), startTag)
          (columnsB, primQueryB, endTag) = Q.runSimpleQueryArr qB ((), midTag)

          (newColumnsA, ljPEsA) =
            PM.run (U.runUnpackspec unpackA (J.extractLeftJoinFields 1 endTag) columnsA)
          (newColumnsB, ljPEsB) =
            PM.run (U.runUnpackspec unpackB (J.extractLeftJoinFields 2 endTag) columnsB)

          nullableColumnsB = J.toNullable nullmaker newColumnsB

          Column cond' = cond (columnsA, columnsB)
          primQueryR = PQ.Join PQ.LeftJoin (ljPEsA ++ ljPEsB) cond' primQueryA primQueryB
