{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Wrapped where

import           Control.Arrow (arr, (<<<))
import qualified Control.Arrow as Arrow
import qualified Control.Category
import           Control.Category (Category)
import qualified Data.Functor.Contravariant as C
import qualified Data.Functor.Contravariant.Divisible as D
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

data WrappedSumProfunctor p a b where
  WrappedSumProfunctor        :: p a b -> WrappedSumProfunctor p a b
  WrappedSumProfunctorId      :: WrappedSumProfunctor p a a
  WrappedSumProfunctorArr     :: (a -> b) -> WrappedSumProfunctor p a b
  WrappedSumProfunctorCompose :: WrappedSumProfunctor p b c
                              -> WrappedSumProfunctor p a b
                              -> WrappedSumProfunctor p a c
  WrappedSumProfunctorChoice  ::
       WrappedSumProfunctor p a a'
    -> WrappedSumProfunctor p b b'
    -> WrappedSumProfunctor p (Either a b) (Either a' b')

newtype WrappedDecidable f a b =
  WrappedDecidable { unWrappedDecidable :: f a }

instance C.Contravariant f => P.Profunctor (WrappedDecidable f) where
  dimap f _ = WrappedDecidable . C.contramap f . unWrappedDecidable

instance D.Decidable f => PP.SumProfunctor (WrappedDecidable f) where
  f1 +++! f2 =
    WrappedDecidable (D.choose id (unWrappedDecidable f1)
                                  (unWrappedDecidable f2))

constructor :: P.Profunctor p
            => (b -> c) -> p a b -> WrappedSumProfunctor p a c
constructor c p = P.rmap c (WrappedSumProfunctor p)

constructorDecidable :: D.Decidable f
                     => f a
                     -> WrappedSumProfunctor (WrappedDecidable f) a c
constructorDecidable f = WrappedSumProfunctor (WrappedDecidable f)

asSumProfunctor :: PP.SumProfunctor p
                => WrappedSumProfunctor p a b -> p a b
asSumProfunctor w = case unWrappedSumProfunctorE w of
  Left p  -> p
  Right _ -> error "unWrappedSumProfunctor was function"

unWrappedSumProfunctorE :: PP.SumProfunctor p
                        => WrappedSumProfunctor p a b -> Either (p a b) (a -> b)
unWrappedSumProfunctorE = \case
  WrappedSumProfunctor p    -> Left p
  WrappedSumProfunctorId    -> Right id
  WrappedSumProfunctorArr f -> Right f
  WrappedSumProfunctorCompose w1 w2 ->
    case (unWrappedSumProfunctorE w1, unWrappedSumProfunctorE w2) of
      (Left _, Left _)     -> error "Composing two profunctors"
      (Right f, Left p)    -> Left (P.rmap f p)
      (Left p, Right f)    -> Left (P.lmap f p)
      (Right f1, Right f2) -> Right (f1 . f2)

  WrappedSumProfunctorChoice w1 w2 ->
    case (unWrappedSumProfunctorE w1, unWrappedSumProfunctorE w2) of
      (Left p1, Left p2) -> Left (p1 PP.+++! p2)
      _ -> error "WrappedSumProfunctorChoice"

instance Category (WrappedSumProfunctor p) where
  id  = WrappedSumProfunctorId
  (.) = WrappedSumProfunctorCompose

instance Arrow.Arrow (WrappedSumProfunctor p) where
  arr   = WrappedSumProfunctorArr
  first = error "WrappedSumProfunctor first"

instance PP.SumProfunctor p => Arrow.ArrowChoice (WrappedSumProfunctor p) where
  (+++) = WrappedSumProfunctorChoice

instance P.Profunctor p => P.Profunctor (WrappedSumProfunctor p) where
  dimap f g w = arr g <<< w <<< arr f
