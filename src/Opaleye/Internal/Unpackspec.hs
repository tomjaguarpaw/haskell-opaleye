{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.Unpackspec where

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Column as C

import           Control.Applicative (Applicative, pure, (<*>))
import           Data.Functor.Identity (Identity(Identity), runIdentity)
import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

newtype Unpackspec columns columns' =
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
  Unpackspec (PM.PackMapColumn Identity Identity columns columns')

-- | Target the single 'HPQ.PrimExpr' inside a 'C.Column'
unpackspecColumn :: Unpackspec (C.Column a) (C.Column a)
unpackspecColumn = Unpackspec PM.pmColumn

-- | Modify all the targeted 'HPQ.PrimExpr's
runUnpackspec :: Applicative f
                 => Unpackspec columns b
                 -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                 -> columns -> f b
runUnpackspec (Unpackspec (PM.PackMapColumn f)) g =
  fmap runIdentity
  . PM.traversePM f (fmap Identity . g . runIdentity)
  . Identity

-- | Extract all the targeted 'HPQ.PrimExpr's
collectPEs :: Unpackspec s t -> s -> [HPQ.PrimExpr]
collectPEs unpackspec = fst . runUnpackspec unpackspec f
  where f pe = ([pe], pe)

instance D.Default Unpackspec (C.Column a) (C.Column a) where
  def = unpackspecColumn

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (Unpackspec a) where
  fmap f (Unpackspec g) = Unpackspec (fmap f g)

instance Applicative (Unpackspec a) where
  pure = Unpackspec . pure
  Unpackspec f <*> Unpackspec x = Unpackspec (f <*> x)

instance Profunctor Unpackspec where
  dimap f g (Unpackspec q) = Unpackspec (dimap f g q)

instance ProductProfunctor Unpackspec where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor Unpackspec where
  Unpackspec x1 +++! Unpackspec x2 = Unpackspec (x1 PP.+++! x2)

--}
