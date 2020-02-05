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

  E :: Combinator ('BasicType (a -> b) ':-> 'BasicType a ':-> 'BasicType b)

type B1 a = 'E ':* 'B0 a

type B1' a b = ('E ':* 'B0 a) ':* b

type family Reduce (arg1 :: Combinator a) :: Combinator a

type instance Reduce ('B0 a) = 'B0 a

type instance Reduce 'I = 'I
type instance Reduce 'K = 'K
type instance Reduce 'S = 'S
type instance Reduce 'E = 'E

type instance Reduce ('I ':* a) = Reduce a
type instance Reduce ('K ':* a) = 'K ':* a
type instance Reduce ('S ':* a) = 'S ':* a
type instance Reduce ('E ':* a) = 'E ':* a

type instance Reduce ('I ':* a ':* b) = Reduce (Reduce ('I ':* a) ':* b)
type instance Reduce ('K ':* a ':* b) = Reduce a
type instance Reduce ('S ':* a ':* b) = 'S ':* a ':* b
type instance Reduce ('E ':* a ':* b) = 'B0 (Basic (Reduce a) (Basic (Reduce b)))

type instance Reduce ('I ':* a ':* b ':* c) =
  Reduce (Reduce ('I ':* a) ':* b ':* c)
type instance Reduce ('K ':* a ':* b ':* c) =
  Reduce (Reduce ('K ':* a ':* b) ':* c)
type instance Reduce ('S ':* a ':* b ':* c) =
  Reduce ((a ':* c) ':* (b ':* c))

type instance Reduce (a ':* b ':* c ':* d ':* e) =
  Reduce (Reduce (Reduce (Reduce (a ':* b) ':* c) ':* d) ':* e)

type family Basic (arg1 :: Combinator ('BasicType a)) :: a

type instance Basic ('B0 a) = a

data (:~:) a b where
  Refl :: a :~: a

basic :: Basic ('B0 a) :~: a
basic = Refl

kT1 :: Basic (Reduce ('K ':* 'B0 a ':* b)) :~: a
kT1 = Refl

kT :: Basic (Reduce ('K ':* B1 a ':* b ':* 'B0 c)) :~: a c
kT = Refl

i :: a -> a
i a = a

k :: a -> b -> a
k a _b = a

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g a = f a (g a)



data ToArrow l a where
  TABasicType :: a -> ToArrow '[] a
  TAArrowType :: ToArrow as (a -> k)
              -> ToArrow (a ': as) k

data FromArrow l a where
  FromArrow :: ToArrow as k
            -> FromArrow as ('BasicType k)
  PullArrow :: FromArrow (a ': as) k
            -> FromArrow as ('BasicType a ':-> k)

example :: ToArrow '[a1, a2] ()
example =
  (TAArrowType (TAArrowType (TABasicType (\_ _ -> ()))))
