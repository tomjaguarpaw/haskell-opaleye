{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}

module Opaleye.Internal.TypeFamilies2
  (module Opaleye.Internal.TypeFamilies2, NN, N, Req, Opt)
where

import           Opaleye.Column (Column, Nullable)
import qualified Opaleye.Field as F
import           Opaleye.Internal.TypeFamilies (N, NN,Optionality, Opt, Req)

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

  B :: a -> Combinator ('BasicType a)

  U :: Combinator ('BasicType (w a) ':-> 'BasicType a)

  E :: Combinator ('BasicType (a -> b) ':-> 'BasicType a ':-> 'BasicType b)

type B0 = 'B

type B1 a = 'E ':* 'B a
type B2 a = Fmap 'E ':* B1 a
type B3 a = Fmap (Fmap 'E) ':* B2 a
type B4 a = Fmap (Fmap (Fmap 'E)) ':* B3 a

type Pure a = 'K ':* a
type f :<*> x = 'S ':* f ':* x
type Compose = 'S ':* ('K ':* 'S) ':* 'K
type Fmap f = Compose ':* f
type f :<$> x = Fmap f ':* x

type family Reduce (arg1 :: Combinator a) :: Combinator a

type instance Reduce ('B a) = 'B a

type instance Reduce 'I = 'I
type instance Reduce 'K = 'K
type instance Reduce 'S = 'S
type instance Reduce 'U = 'U
type instance Reduce 'E = 'E

type instance Reduce ('I ':* a) = Reduce a
type instance Reduce ('K ':* a) = 'K ':* a
type instance Reduce ('S ':* a) = 'S ':* a
type instance Reduce ('U ':* a) = 'B (Unwrap (Basic (Reduce a)))
type instance Reduce ('E ':* a) = 'E ':* a

type instance Reduce ('I ':* a ':* b) = Reduce (Reduce ('I ':* a) ':* b)
type instance Reduce ('K ':* a ':* b) = Reduce a
type instance Reduce ('S ':* a ':* b) = 'S ':* a ':* b
type instance Reduce ('E ':* a ':* b) = 'B (Basic (Reduce a) (Basic (Reduce b)))

type instance Reduce ('I ':* a ':* b ':* c) =
  Reduce (Reduce ('I ':* a) ':* b ':* c)
type instance Reduce ('K ':* a ':* b ':* c) =
  Reduce (Reduce ('K ':* a ':* b) ':* c)
type instance Reduce ('S ':* a ':* b ':* c) =
  Reduce ((a ':* c) ':* (b ':* c))

type instance Reduce (a ':* b ':* c ':* d ':* e) =
  Reduce (Reduce (Reduce (Reduce (a ':* b) ':* c) ':* d) ':* e)

type family Basic (arg1 :: Combinator ('BasicType a)) :: a

type instance Basic ('B a) = a

type family Unwrap (arg1 :: w a) :: a

data TupleSelector a where
  FstG :: (a, b) -> TupleSelector a
  SndG :: (a, b) -> TupleSelector b

type instance Unwrap ('FstG '(a, b)) = a
type instance Unwrap ('SndG '(a, b)) = b

type Fst = Fmap 'U ':* B1 'FstG
type Snd = Fmap 'U ':* B1 'SndG

data (:~:) a b where
  Refl :: a :~: a

basic :: Basic ('B a) :~: a
basic = Refl

kT1 :: Basic (Reduce ('K ':* 'B a ':* b)) :~: a
kT1 = Refl

kT :: Basic (Reduce ('K ':* B1 f ':* b ':* 'B c)) :~: f c
kT = Refl

kTuple :: Basic (Reduce (Fst ':* 'B '(a, b))) :~: a
kTuple = Refl

kT4 :: Basic (Reduce (B4 f ':* 'B a ':* 'B b ':* 'B c ':* 'B d)) :~: f a b c d
kT4 = Refl

data C a  = C a a F.Nullability
data TC a = TC (C a) Optionality

data CSelector a where
  HaskellType      :: C a -> CSelector a
  OpaleyeType      :: C a -> CSelector a
  OpaleyeNullsType :: C a -> CSelector a

data TCSelector a where
  CSelect      :: TC a -> TCSelector (C a)
  OptionalityS :: TC a -> TCSelector a

type instance Unwrap ('HaskellType ('C h o NN)) = h
type instance Unwrap ('HaskellType ('C h o N)) =  Maybe h
type instance Unwrap ('OpaleyeType ('C h o NN)) = Column o
type instance Unwrap ('OpaleyeType ('C h o N)) =  Column (Nullable o)
type instance Unwrap ('OpaleyeNullsType ('C h o n)) = Column (Nullable o)

type instance Unwrap ('CSelect ('TC c o)) = c
type instance Unwrap ('OptionalityS ('TC c o)) = o

type RecordField a b c = 'B ('C a b c)
type TableRecordField a b c o = 'B ('TC ('C a b c) o)

type f :** x = Basic (Reduce (f ':* x))

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
