{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.SQLite.Binary where

import           Opaleye.SQLite.QueryArr (Query)
import qualified Opaleye.SQLite.Internal.QueryArr as Q
import qualified Opaleye.SQLite.Internal.Binary as B
import qualified Opaleye.SQLite.Internal.Tag as T
import qualified Opaleye.SQLite.Internal.PrimQuery as PQ
import qualified Opaleye.SQLite.Internal.PackMap as PM

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
unionAllExplicit binaryspec q1 q2 = Q.simpleQueryArr q where
  q ((), startTag) = (newColumns, newPrimQuery, T.next endTag)
    where (columns1, primQuery1, midTag) = Q.runSimpleQueryArr q1 ((), startTag)
          (columns2, primQuery2, endTag) = Q.runSimpleQueryArr q2 ((), midTag)

          (newColumns, pes) =
            PM.run (B.runBinaryspec binaryspec (B.extractBinaryFields endTag)
                                    (columns1, columns2))

          newPrimQuery = PQ.Binary PQ.UnionAll pes (primQuery1, primQuery2)
