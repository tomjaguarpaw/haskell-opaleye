{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary 
  ( union
  , unionAll
  , unionExplicit
  , unionAllExplicit
  ) where

import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Binary as B
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM

import           Data.Profunctor.Product.Default (Default, def)

-- | Example type specialization:
--
-- @
-- unionAll :: Query (Column a, Column b)
--          -> Query (Column a, Column b)
--          -> Query (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- unionAll :: Query (Foo (Column a) (Column b) (Column c))
--          -> Query (Foo (Column a) (Column b) (Column c))
--          -> Query (Foo (Column a) (Column b) (Column c))
-- @
unionAll :: Default B.Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
unionAll = unionAllExplicit def

unionAllExplicit :: B.Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
unionAllExplicit = sameTypeBinOpHelper PQ.UnionAll


-- | The same as unionAll, except that it additionally removes any 
--   duplicate rows.
union :: Default B.Binaryspec columns columns =>
         Query columns -> Query columns -> Query columns
union = unionExplicit def

unionExplicit :: B.Binaryspec columns columns'
              -> Query columns -> Query columns -> Query columns'
unionExplicit = sameTypeBinOpHelper PQ.Union


sameTypeBinOpHelper :: PQ.BinOp -> B.Binaryspec columns columns'
                    -> Query columns -> Query columns -> Query columns'
sameTypeBinOpHelper binop binaryspec q1 q2 = Q.simpleQueryArr q where
  q ((), startTag) = (newColumns, newPrimQuery, T.next endTag)
    where (columns1, primQuery1, midTag) = Q.runSimpleQueryArr q1 ((), startTag)
          (columns2, primQuery2, endTag) = Q.runSimpleQueryArr q2 ((), midTag)

          (newColumns, pes) =
            PM.run (B.runBinaryspec binaryspec (B.extractBinaryFields endTag)
                                    (columns1, columns2))

          newPrimQuery = PQ.Binary binop pes (primQuery1, primQuery2)
  



