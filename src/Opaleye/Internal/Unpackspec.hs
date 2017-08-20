{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.Unpackspec where

import qualified Opaleye.Internal.PackMapColumn as PM
import qualified Opaleye.Column as C

import           Control.Applicative (Applicative)
import           Data.Functor.Identity (Identity(Identity), runIdentity)

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- | An 'Unpackspec' @columns@ @columns'@ allows you to extract and
-- modify a sequence of 'HPQ.PrimExpr's inside a value of type
-- @columns@.
--
-- For example, the 'Default' instance of type 'Unpackspec' @(Column
-- a, Column b)@ @(Column a, Column b)@ allows you to manipulate or
-- extract the two 'HPQ.PrimExpr's inside a @(Column a, Column b)@.  The
-- 'Default' instance of type @Foo (Column a) (Column b) (Column c)@
-- will allow you to manipulate or extract the three 'HPQ.PrimExpr's
-- contained therein (for a user-defined product type @Foo@, assuming
-- the @makeAdaptorAndInstance@ splice from
-- @Data.Profunctor.Product.TH@ has been run).
--
-- Users should almost never need to create or manipulate
-- `Unpackspec`s.  Typically they will be created automatically by
-- the 'D.Default' instance.  If you really need to you can create
-- 'Unpackspec's by hand using 'unpackspecColumn' and the
-- 'Profunctor', 'ProductProfunctor' and 'SumProfunctor' operations.
type Unpackspec = PM.PackMapColumn Identity

-- | Target the single 'HPQ.PrimExpr' inside a 'C.Column'
unpackspecColumn :: Unpackspec (C.Column a) (C.Column a)
unpackspecColumn = PM.pmColumn

-- | Modify all the targeted 'HPQ.PrimExpr's
runUnpackspec :: Applicative f
                 => Unpackspec columns b
                 -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                 -> columns -> f b
runUnpackspec u = PM.runPMC Identity runIdentity u

-- | Extract all the targeted 'HPQ.PrimExpr's
collectPEs :: Unpackspec s t -> s -> [HPQ.PrimExpr]
collectPEs unpackspec = fst . runUnpackspec unpackspec f
  where f pe = ([pe], pe)
