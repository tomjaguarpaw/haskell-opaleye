-- | Binary relational operations on 'S.Select's, that is, operations
-- which take two 'S.Select's as arguments and return a single 'S.Select'.
--
-- All the binary relational operations have the same type
-- specializations.  For example:
--
-- @
-- unionAll :: S.Select (Column a, Column b)
--          -> S.Select (Column a, Column b)
--          -> S.Select (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- unionAll :: S.Select (Foo (Column a) (Column b) (Column c))
--          -> S.Select (Foo (Column a) (Column b) (Column c))
--          -> S.Select (Foo (Column a) (Column b) (Column c))
-- @
--
-- Please note that by design there are no binary relational functions
-- of type @S.SelectArr a b -> S.SelectArr a b -> S.SelectArr a b@.  Such
-- functions would allow violation of SQL's scoping rules and lead to
-- invalid queries.
--
-- `unionAll` is very close to being the @\<|\>@ operator of a
-- @Control.Applicative.Alternative@ instance but it fails to work
-- only because of the typeclass constraint it has.

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary where

import qualified Opaleye.Internal.Binary as B
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Select             as S

import           Data.Profunctor.Product.Default (Default, def)

-- * Binary operations

unionAll :: Default B.Binaryspec fields fields =>
            S.Select fields -> S.Select fields -> S.Select fields
unionAll = unionAllExplicit def

-- | The same as unionAll, except that it additionally removes any
--   duplicate rows.
union :: Default B.Binaryspec fields fields =>
         S.Select fields -> S.Select fields -> S.Select fields
union = unionExplicit def

intersectAll :: Default B.Binaryspec fields fields =>
            S.Select fields -> S.Select fields -> S.Select fields
intersectAll = intersectAllExplicit def

-- | The same as intersectAll, except that it additionally removes any
--   duplicate rows.
intersect :: Default B.Binaryspec fields fields =>
         S.Select fields -> S.Select fields -> S.Select fields
intersect = intersectExplicit def

exceptAll :: Default B.Binaryspec fields fields =>
            S.Select fields -> S.Select fields -> S.Select fields
exceptAll = exceptAllExplicit def

-- | The same as exceptAll, except that it additionally removes any
--   duplicate rows.
except :: Default B.Binaryspec fields fields =>
         S.Select fields -> S.Select fields -> S.Select fields
except = exceptExplicit def

-- * Explicit versions

unionAllExplicit :: B.Binaryspec fields fields'
                 -> S.Select fields -> S.Select fields -> S.Select fields'
unionAllExplicit = B.sameTypeBinOpHelper PQ.UnionAll

unionExplicit :: B.Binaryspec fields fields'
              -> S.Select fields -> S.Select fields -> S.Select fields'
unionExplicit = B.sameTypeBinOpHelper PQ.Union

intersectAllExplicit :: B.Binaryspec fields fields'
                 -> S.Select fields -> S.Select fields -> S.Select fields'
intersectAllExplicit = B.sameTypeBinOpHelper PQ.IntersectAll

intersectExplicit :: B.Binaryspec fields fields'
              -> S.Select fields -> S.Select fields -> S.Select fields'
intersectExplicit = B.sameTypeBinOpHelper PQ.Intersect

exceptAllExplicit :: B.Binaryspec fields fields'
                 -> S.Select fields -> S.Select fields -> S.Select fields'
exceptAllExplicit = B.sameTypeBinOpHelper PQ.ExceptAll

exceptExplicit :: B.Binaryspec fields fields'
              -> S.Select fields -> S.Select fields -> S.Select fields'
exceptExplicit = B.sameTypeBinOpHelper PQ.Except
