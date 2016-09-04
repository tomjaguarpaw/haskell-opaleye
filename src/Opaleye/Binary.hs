-- | Binary relational operations on 'Query's, that is, operations
-- which take two 'Query's as arguments and return a single 'Query'.
--
-- All the binary relational operations have the same type
-- specializations.  For example:
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
-- Please note that by design there are no binary relational functions
-- of type @QueryArr a b -> QueryArr a b -> QueryArr a b@.  Such
-- functions would allow violation of SQL's scoping rules and lead to
-- invalid queries.
--
-- `unionAll` is very close to being the @<|>@ operator of a
-- @Control.Applicative.Alternative@ instance but it fails to work
-- only because of the typeclass constraint it has.

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary where

import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.Binary as B
import qualified Opaleye.Internal.PrimQuery as PQ

import           Data.Profunctor.Product.Default (Default, def)

unionAll :: Default B.Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
unionAll = unionAllExplicit def

-- | The same as unionAll, except that it additionally removes any
--   duplicate rows.
union :: Default B.Binaryspec columns columns =>
         Query columns -> Query columns -> Query columns
union = unionExplicit def

intersectAll :: Default B.Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
intersectAll = intersectAllExplicit def

-- | The same as intersectAll, except that it additionally removes any
--   duplicate rows.
intersect :: Default B.Binaryspec columns columns =>
         Query columns -> Query columns -> Query columns
intersect = intersectExplicit def

exceptAll :: Default B.Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
exceptAll = exceptAllExplicit def

-- | The same as exceptAll, except that it additionally removes any
--   duplicate rows.
except :: Default B.Binaryspec columns columns =>
         Query columns -> Query columns -> Query columns
except = exceptExplicit def


unionAllExplicit :: B.Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
unionAllExplicit = B.sameTypeBinOpHelper PQ.UnionAll

unionExplicit :: B.Binaryspec columns columns'
              -> Query columns -> Query columns -> Query columns'
unionExplicit = B.sameTypeBinOpHelper PQ.Union

intersectAllExplicit :: B.Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
intersectAllExplicit = B.sameTypeBinOpHelper PQ.IntersectAll

intersectExplicit :: B.Binaryspec columns columns'
              -> Query columns -> Query columns -> Query columns'
intersectExplicit = B.sameTypeBinOpHelper PQ.Intersect

exceptAllExplicit :: B.Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
exceptAllExplicit = B.sameTypeBinOpHelper PQ.ExceptAll

exceptExplicit :: B.Binaryspec columns columns'
              -> Query columns -> Query columns -> Query columns'
exceptExplicit = B.sameTypeBinOpHelper PQ.Except
