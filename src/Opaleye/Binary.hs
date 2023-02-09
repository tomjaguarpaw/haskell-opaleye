-- | Binary relational operations on 'S.Select's, that is, operations
-- which take two 'S.Select's as arguments and return a single 'S.Select'.
--
-- All the binary relational operations have the same type
-- specializations.  For example:
--
-- @
-- unionAll :: S.Select (Field a, Field b)
--          -> S.Select (Field a, Field b)
--          -> S.Select (Field a, Field b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- unionAll :: S.Select (Foo (Field a) (Field b) (Field c))
--          -> S.Select (Foo (Field a) (Field b) (Field c))
--          -> S.Select (Foo (Field a) (Field b) (Field c))
-- @
--
-- If you want to run a binary relational operator on
-- 'Select.SelectArr's you should apply 'Opaleye.Lateral.bilaterally'
-- to it, for example
--
-- @
-- 'Opaleye.Lateral.bilaterally' 'union'
--   :: 'Data.Profunctor.Product.Default' 'B.Binaryspec' fields fields
--   => 'S.SelectArr' i fields -> 'S.SelectArr' i fields -> 'S.SelectArr' i fields
-- @
--
-- `unionAll` is very close to being the @\<|\>@ operator of a
-- @Control.Applicative.Alternative@ instance but it fails to work
-- only because of the typeclass constraint it has.

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary (-- * Binary operations
                       unionAll,
                       union,
                       intersectAll,
                       intersect,
                       exceptAll,
                       except,
                       -- * Explicit versions
                       unionAllExplicit,
                       unionExplicit,
                       intersectAllExplicit,
                       intersectExplicit,
                       exceptAllExplicit,
                       exceptExplicit,
                       -- * Adaptors
                       binaryspecField,
                      ) where

import qualified Opaleye.Internal.Binary as B
import qualified Opaleye.Internal.Column
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Select             as S

import           Data.Profunctor.Product.Default (Default, def)

unionAll :: Default B.Binaryspec fields fields =>
            S.Select fields -> S.Select fields -> S.Select fields
unionAll = unionAllExplicit def

-- | The same as 'unionAll', except that it additionally removes any
--   duplicate rows.
union :: Default B.Binaryspec fields fields =>
         S.Select fields -> S.Select fields -> S.Select fields
union = unionExplicit def

intersectAll :: Default B.Binaryspec fields fields =>
            S.Select fields -> S.Select fields -> S.Select fields
intersectAll = intersectAllExplicit def

-- | The same as 'intersectAll', except that it additionally removes any
--   duplicate rows.
intersect :: Default B.Binaryspec fields fields =>
         S.Select fields -> S.Select fields -> S.Select fields
intersect = intersectExplicit def

exceptAll :: Default B.Binaryspec fields fields =>
            S.Select fields -> S.Select fields -> S.Select fields
exceptAll = exceptAllExplicit def

-- | The same as 'exceptAll', except that it additionally removes any
--   duplicate rows.
except :: Default B.Binaryspec fields fields =>
         S.Select fields -> S.Select fields -> S.Select fields
except = exceptExplicit def

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

binaryspecField :: (B.Binaryspec
                        (Opaleye.Internal.Column.Field_ n a)
                        (Opaleye.Internal.Column.Field_ n a))
binaryspecField = B.binaryspecColumn
