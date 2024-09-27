{-# OPTIONS_HADDOCK not-home #-}

module Opaleye.Internal.Unpackspec where

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Field as F

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor)
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

newtype Unpackspec fields fields' =
  -- | An 'Unpackspec' @fields@ @fields'@ allows you to extract and
  -- modify a sequence of 'HPQ.PrimExpr's inside a value of type
  -- @fields@.
  --
  -- For example, the 'Default' instance of type 'Unpackspec' @(Field
  -- a, Field b)@ @(Field a, Field b)@ allows you to manipulate or
  -- extract the two 'HPQ.PrimExpr's inside a @(Field a, Field b)@.  The
  -- 'Default' instance of type @Foo (Field a) (Field b) (Field c)@
  -- will allow you to manipulate or extract the three 'HPQ.PrimExpr's
  -- contained therein (for a user-defined product type @Foo@, assuming
  -- the @makeAdaptorAndInstanceInferrable@ splice from
  -- @Data.Profunctor.Product.TH@ has been run).
  --
  -- Users should almost never need to create or manipulate
  -- `Unpackspec`s.  Typically they will be created automatically by
  -- the 'D.Default' instance.  If you really need to you can create
  -- 'Unpackspec's by hand using 'unpackspecField' and the
  -- 'Profunctor', 'ProductProfunctor' and 'SumProfunctor' operations.
  Unpackspec (PM.PackMap HPQ.PrimExpr HPQ.PrimExpr fields fields')

-- | Target the single 'HPQ.PrimExpr' inside a 'F.Field n'
unpackspecField :: Unpackspec (F.Field_ n a) (F.Field_ n a)
unpackspecField = dimap IC.unColumn IC.Column (Unpackspec (PM.PackMap id))

-- | Modify all the targeted 'HPQ.PrimExpr's
runUnpackspec :: Applicative f
                 => Unpackspec columns b
                 -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                 -> columns -> f b
runUnpackspec (Unpackspec f) = PM.traversePM f

-- | Extract all the targeted 'HPQ.PrimExpr's
collectPEs :: Unpackspec s t -> s -> [HPQ.PrimExpr]
collectPEs unpackspec = fst . runUnpackspec unpackspec f
  where f pe = ([pe], pe)

instance D.Default Unpackspec (F.Field_ n a) (F.Field_ n a) where
  def = unpackspecField

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
  purePP = pure
  (****) = (<*>)

instance PP.SumProfunctor Unpackspec where
  Unpackspec x1 +++! Unpackspec x2 = Unpackspec (x1 PP.+++! x2)

--}
