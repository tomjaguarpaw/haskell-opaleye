{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Opaleye.Test.Fields where

import           Prelude hiding (compare)

import           Wrapped (constructor, asSumProfunctor,
                          constructorDecidable, asDecidable)

import qualified Opaleye as O
import qualified Opaleye.Internal.MaybeFields as OM
import qualified Opaleye.Internal.Values as OV

import           Control.Arrow ((>>>))
import           Control.Applicative (pure)
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Functor.Contravariant.Divisible as Divisible
import qualified Data.Maybe as Maybe

data Choice i b s = CInt i | CBool b | CString s deriving (Show, Eq, Ord)

chooseChoice :: Divisible.Decidable f
             => (a -> Choice i b s) -> f i -> f b -> f s -> f a
chooseChoice choose fi fb fs = asDecidable $ proc a -> case choose a of
  CInt i    -> constructorDecidable fi -< i
  CBool b   -> constructorDecidable fb -< b
  CString s -> constructorDecidable fs -< s

newtype Choices m i b s =
  Choices { unChoices :: [Either (Choice i b s) (m (Choices m i b s))] }

deriving instance Show Haskells
deriving instance Eq Haskells
deriving instance Ord Haskells

type SimpleField = Choice (O.Field O.SqlInt4)
                          (O.Field O.SqlBool)
                          (O.Field O.SqlText)
type Fields = Choices O.MaybeFields (O.Field O.SqlInt4)
                                    (O.Field O.SqlBool)
                                    (O.Field O.SqlText)
type Haskells = Choices Maybe Int Bool String

emptyChoices :: Choices m i b s
emptyChoices = Choices []

appendChoices :: Choices m i b s -> Choices m i b s -> Choices m i b s
appendChoices c1 c2 = Choices (unChoices c1 ++ unChoices c2)

ppChoices :: (PP.SumProfunctor p, PP.ProductProfunctor p)
          => p (Choice i b s) (Choice i' b' s')
          -> (p (Choices m i b s) (Choices m' i' b' s')
             -> p (m (Choices m i b s)) (m' (Choices m' i' b' s')))
          -> p (Choices m i b s) (Choices m' i' b' s')
ppChoices p f = ps
  where ps = P.dimap unChoices Choices (PP.list (p PP.+++! f ps))

fieldsOfHaskells :: Haskells -> Fields
fieldsOfHaskells = O.toFieldsExplicit toFieldsFields

fieldsList :: Functor m => (a, b, m s) -> Choices m a b s
fieldsList (x, y, ms) =
  Choices [ Left (CInt x),
            Left (CBool y),
            Right (fmap (Choices . pure . Left . CString) ms)
          ]

type FieldsTuple = (O.Field O.SqlInt4,
                    O.Field O.SqlBool,
                    O.MaybeFields (O.Field O.SqlText))
type HaskellsTuple = (Int, Bool, Maybe String)

listFieldsG :: Functor m
            => Choices m i b s -> i -> b -> s -> m s -> (i, b, m s)
listFieldsG f i b s ms = (fst (firstIntOr i f),
                          fst (firstBoolOrTrue b f),
                          ms')
  where ms' = maybe ms (fmap (fst . firstStringOr s)) (firstMaybe f)

listFields :: Fields -> FieldsTuple
listFields f =
  listFieldsG f 1 (O.sqlBool True) (O.sqlString "xyz") O.nothingFields

listHaskells :: Haskells -> HaskellsTuple
listHaskells f = listFieldsG f 1 True "xyz" Nothing

fieldsToMaybeFields :: Applicative m => Choices m i b s -> m (Choices m i b s)
fieldsToMaybeFields fs = case Maybe.listToMaybe (subMaybeFields fs) of
  Nothing -> pure fs
  Just x  -> x

unpackFields :: O.Unpackspec Fields Fields
unpackFields = defChoicesPP O.unpackspecMaybeFields

distinctNullsFields :: OM.WithNulls O.Distinctspec Fields Fields
distinctNullsFields =
  ppChoices defChoicePP (OM.mapMaybeFieldsWithNulls D.def)

distinctFields :: O.Distinctspec Fields Fields
distinctFields = P.dimap unChoices Choices (PP.list
    (defChoicePP PP.+++! OM.unWithNulls D.def distinctNullsFields))

fromFieldsFields :: O.FromFields Fields Haskells
fromFieldsFields = defChoicesPP O.fromFieldsMaybeFields

toFieldsFields :: O.ToFields Haskells Fields
toFieldsFields =
  defChoicesPP (O.toFieldsMaybeFields (fmap Choices OV.nullspecList))

choicePP :: PP.SumProfunctor p
         => p i1 i2 -> p b1 b2 -> p s1 s2
         -> p (Choice i1 b1 s1) (Choice i2 b2 s2)
choicePP p1 p2 p3 = asSumProfunctor $ proc choice -> case choice of
  CInt i    -> constructor CInt    p1 -< i
  CBool b   -> constructor CBool   p2 -< b
  CString s -> constructor CString p3 -< s

defChoicesPP :: (D.Default p a a', D.Default p b b', D.Default p s s',
                 PP.SumProfunctor p, PP.ProductProfunctor p)
             => (p (Choices m a b s) (Choices m' a' b' s')
                -> p (m (Choices m a b s)) (m' (Choices m' a' b' s')))
             -> p (Choices m a b s) (Choices m' a' b' s')
defChoicesPP = ppChoices defChoicePP

defChoicePP :: (D.Default p a a', D.Default p b b', D.Default p s s',
                PP.SumProfunctor p, PP.ProductProfunctor p)
            => p (Choice a b s) (Choice a' b' s')
defChoicePP = choicePP D.def D.def D.def

-- We could try to be clever and look inside the MaybeFields, but this
-- will do for now.
firstBoolOrTrue :: b -> Choices m a b s -> (b, Choices m a b s)
firstBoolOrTrue true c = (b, c)
  where b = case Maybe.mapMaybe (either isBool (const Nothing)) (unChoices c) of
          []    -> true
          (x:_) -> x

-- We could try to be clever and look inside the MaybeFields, but this
-- will do for now.
firstIntOr :: a -> Choices m a b s -> (a, Choices m a b s)
firstIntOr else_ c = (b, c)
  where b = case Maybe.mapMaybe (either isInt (const Nothing)) (unChoices c) of
          []    -> else_
          (x:_) -> x

-- We could try to be clever and look inside the MaybeFields, but this
-- will do for now.
firstStringOr :: s -> Choices m a b s -> (s, Choices m a b s)
firstStringOr else_ c = (b, c)
  where b = case Maybe.mapMaybe (either isString (const Nothing)) (unChoices c) of
          []    -> else_
          (x:_) -> x

firstMaybe :: Choices m a b s
           -> Maybe (m (Choices m a b s))
firstMaybe c = case Maybe.mapMaybe (either (const Nothing) Just) (unChoices c) of
          []    -> Nothing
          (x:_) -> Just x

isBool :: Choice a b s -> Maybe b
isBool (CInt _)  = Nothing
isBool (CBool l) = Just l
isBool (CString _) = Nothing

isInt :: Choice a b s -> Maybe a
isInt (CInt a)  = Just a
isInt (CBool _) = Nothing
isInt (CString _) = Nothing

isString :: Choice a b s -> Maybe s
isString (CInt _)  = Nothing
isString (CBool _) = Nothing
isString (CString s) = Just s

pairColumns :: Choices m i b s -> (Choices m i b s, Choices m i b s)
pairColumns cs = (evens cs, odds cs)

unpairColums :: (Choices m i b s, Choices m i b s) -> Choices m i b s
unpairColums = uncurry appendChoices

odds :: Choices m i b s -> Choices m i b s
odds (Choices [])     = Choices []
odds (Choices (x:xs)) = Choices (x : unChoices (evens (Choices xs)))

evens :: Choices m i b s -> Choices m i b s
evens (Choices [])     = Choices []
evens (Choices (_:xs)) = odds (Choices xs)

subMaybeFields :: Choices m i b s -> [m (Choices m i b s)]
subMaybeFields = unChoices >>> Maybe.mapMaybe (\case Left _  -> Nothing
                                                     Right r -> Just r)
