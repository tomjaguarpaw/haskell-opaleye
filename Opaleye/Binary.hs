{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary where

import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Binary as B
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM

import           Data.Profunctor.Product.Default (Default, def)

unionAll :: Default B.Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
unionAll = unionAllExplicit def

unionAllExplicit :: B.Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
unionAllExplicit binaryspec q1 q2 = Q.simpleQueryArr q where
  q ((), startTag) = (newColumns, newPrimQuery, T.next endTag)
    where (columns1, primQuery1, midTag) = Q.runSimpleQueryArr q1 ((), startTag)
          (columns2, primQuery2, endTag) = Q.runSimpleQueryArr q2 ((), midTag)

          (newColumns, pes) =
            PM.run (B.runBinaryspec binaryspec (B.extractBinaryFields endTag)
                                    (columns1, columns2))

          newPrimQuery = PQ.Binary PQ.UnionAll pes (primQuery1, primQuery2)
