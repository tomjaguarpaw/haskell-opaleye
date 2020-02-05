{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}

module Opaleye.Internal.TypeFamilies2 where

data ArrowType a =
    BasicType a
  | (:->) (ArrowType a) (ArrowType a)

data ToArrow l a where
  TABasicType :: a -> ToArrow '[] a
  TAArrowType :: ToArrow as (a -> k)
              -> ToArrow (a ': as) k

data FromArrow l a where
  FromArrow :: ToArrow as k
            -> FromArrow as ('BasicType k)
  PullArrow :: FromArrow (a ': as) k
            -> FromArrow as ('BasicType a ':-> k)

example :: FromArrow '[] ('BasicType a1 ':-> 'BasicType a2 ':-> 'BasicType ())
example =
  PullArrow (PullArrow (FromArrow (TAArrowType (TAArrowType (TABasicType (\_ _ -> ()))))))


infixr :->
infixl :*

data Combinator a where
  I :: Combinator (a ':-> a)
  K :: Combinator (a ':-> b ':-> a)
  S :: Combinator ((a ':-> b ':-> c) ':-> (a ':-> b) ':-> a ':-> c)

  (:*) :: Combinator (a ':-> b)
       -> Combinator a
       -> Combinator b

  B0 :: a -> Combinator ('BasicType a)
  B1 :: (a -> b) -> Combinator ('BasicType a ':-> 'BasicType b)

  BA :: FromArrow '[] a -> Combinator a

data Step a = Step a | Done a

type family Reduce (arg1 :: Combinator a) :: (Combinator a)

type instance Reduce ('B0 a) = 'B0 a

type instance Reduce 'I = 'I
type instance Reduce 'K = 'K
type instance Reduce 'S = 'S

type instance Reduce ('I ':* a) = Reduce a
type instance Reduce ('K ':* a) = 'K ':* a
type instance Reduce ('S ':* a) = 'S ':* a

type instance Reduce ('B1 f ':* 'B0 b) = 'B0 (f b)
type instance Reduce ('B1 f ':* (a ':* b)) = Reduce ('B1 f ':* Reduce (a ':* b))

type instance Reduce ('I ':* a ':* b) = Reduce (a ':* b)
type instance Reduce ('K ':* a ':* b) = Reduce b
type instance Reduce ('S ':* a ':* b) = 'S ':* a ':* b
type instance Reduce ('I ':* a ':* b ':* c) = Reduce (a ':* b ':* c)
type instance Reduce ('K ':* a ':* b ':* c) = Reduce (a ':* c)
type instance Reduce ('S ':* a ':* b ':* c) = Reduce ((a ':* c) ':* (b ':* c))
type instance Reduce (a ':* b ':* c ':* d ':* e) =
  Reduce (Reduce (Reduce (Reduce (a ':* b) ':* c) ':* d) ':* e)

type family Basic (arg1 :: Combinator ('BasicType a)) :: a

type instance Basic ('B0 a) = a

data (:~:) a b where
  Refl :: a :~: a

basic :: Basic ('B0 a) :~: a
basic = Refl

kT :: Basic (Reduce ('K ':* 'B1 a ':* b ':* 'B0 c)) :~: a c
kT = Refl

i :: a -> a
i a = a

k :: a -> b -> a
k a _b = a

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g a = f a (g a)
