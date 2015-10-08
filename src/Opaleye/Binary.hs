{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary where

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
unionAllExplicit = B.sameTypeBinOpHelper PQ.UnionAll


-- | The same as unionAll, except that it additionally removes any 
--   duplicate rows.
union :: Default B.Binaryspec columns columns =>
         Query columns -> Query columns -> Query columns
union = unionExplicit def

unionExplicit :: B.Binaryspec columns columns'
              -> Query columns -> Query columns -> Query columns'
unionExplicit = B.sameTypeBinOpHelper PQ.Union

