{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary where

import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.Binary as B
import qualified Opaleye.Internal.PrimQuery as PQ

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
--
-- By design there is no union function of type @QueryArr a b ->
-- QueryArr a b -> QueryArr a b@.  Such a function would allow
-- violation of SQL's scoping rules and lead to invalid queries.
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



intersectAll :: Default B.Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
intersectAll = intersectAllExplicit def

intersectAllExplicit :: B.Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
intersectAllExplicit = B.sameTypeBinOpHelper PQ.IntersectAll


-- | The same as intersectAll, except that it additionally removes any
--   duplicate rows.
intersect :: Default B.Binaryspec columns columns =>
         Query columns -> Query columns -> Query columns
intersect = intersectExplicit def

intersectExplicit :: B.Binaryspec columns columns'
              -> Query columns -> Query columns -> Query columns'
intersectExplicit = B.sameTypeBinOpHelper PQ.Intersect


exceptAll :: Default B.Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
exceptAll = exceptAllExplicit def

exceptAllExplicit :: B.Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
exceptAllExplicit = B.sameTypeBinOpHelper PQ.ExceptAll


-- | The same as exceptAll, except that it additionally removes any
--   duplicate rows.
except :: Default B.Binaryspec columns columns =>
         Query columns -> Query columns -> Query columns
except = exceptExplicit def

exceptExplicit :: B.Binaryspec columns columns'
              -> Query columns -> Query columns -> Query columns'
exceptExplicit = B.sameTypeBinOpHelper PQ.Except
