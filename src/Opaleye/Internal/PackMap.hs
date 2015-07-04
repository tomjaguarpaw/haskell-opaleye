{-# LANGUAGE Rank2Types #-}

module Opaleye.Internal.PackMap where

import qualified Opaleye.Internal.Tag as T

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Control.Applicative (Applicative, pure, (<*>), liftA2)
import qualified Control.Monad.Trans.State as State
import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import qualified Data.Functor.Identity as I

-- This is rather like a Control.Lens.Traversal with the type
-- parameters switched but I'm not sure if it should be required to
-- obey the same laws.
--
-- TODO: We could attempt to generalise this to
--
-- data LensLike f a b s t = LensLike ((a -> f b) -> s -> f t)
--
-- i.e. a wrapped, argument-flipped Control.Lens.LensLike
--
-- This would allow us to do the Profunctor and ProductProfunctor
-- instances (requiring just Functor f and Applicative f respectively)
-- and share them between many different restrictions of f.  For
-- example, TableColumnMaker is like a Setter so we would restrict f
-- to the Distributive case.

-- | A 'PackMap' @a@ @b@ @s@ @t@ encodes how an @s@ contains an
-- updatable sequence of @a@ inside it.  Each @a@ in the sequence can
-- be updated to a @b@ (and the @s@ changes to a @t@ to reflect this
-- change of type).
--
-- 'PackMap' is just like a @Traversal@ from the lens package.
-- 'PackMap' has a different order of arguments to @Traversal@ because
-- it typically needs to be made a 'Profunctor' (and indeed
-- 'ProductProfunctor') in @s@ and @t@.  It is unclear at this point
-- whether we want the same @Traversal@ laws to hold or not.  Our use
-- cases may be much more general.
data PackMap a b s t = PackMap (Applicative f =>
                                (a -> f b) -> s -> f t)

-- | Replaces the targeted occurences of @a@ in @s@ with @b@ (changing
-- the @s@ to a @t@ in the process).  This can be done via an
-- 'Applicative' action.
--
-- 'traverse' is just like @traverse@ from the @lens@ package.
-- 'traverse' used to be called @packmap@.
traverse :: Applicative f => PackMap a b s t -> (a -> f b) -> s -> f t
traverse (PackMap f) = f

-- | Modify the targeted occurrences of @a@ in @s@ with @b@ (changing
-- the @s@ to a @t@ in the process).
--
-- 'over' is just like @over@ from the @lens@ pacakge.
over :: PackMap a b s t -> (a -> b) -> s -> t
over p f = I.runIdentity . traverse p (I.Identity . f)


-- {

-- | A helpful monad for writing columns in the AST
type PM a = State.State (a, Int)

new :: PM a String
new = do
  (a, i) <- State.get
  State.put (a, i + 1)
  return (show i)

write :: a -> PM [a] ()
write a = do
  (as, i) <- State.get
  State.put (as ++ [a], i)

run :: PM [a] r -> (r, [a])
run m = (r, as)
  where (r, (as, _)) = State.runState m ([], 0)

-- }


-- { General functions for writing columns in the AST

-- This one ignores the 'a' when making the internal column name.
extractAttr :: String -> T.Tag -> a
               -> PM [(HPQ.Symbol, a)] HPQ.PrimExpr
extractAttr s = extractAttrPE (const (s ++))

-- This one can make the internal column name depend on the 'a' in
-- question (probably a PrimExpr)
extractAttrPE :: (a -> String -> String) -> T.Tag -> a
               -> PM [(HPQ.Symbol, a)] HPQ.PrimExpr
extractAttrPE mkName t pe = do
  i <- new
  let s = HPQ.Symbol (mkName pe i) t
  write (s, pe)
  return (HPQ.AttrExpr s)

-- }

eitherFunction :: Functor f
               => (a -> f b)
               -> (a' -> f b')
               -> Either a a'
               -> f (Either b b')
eitherFunction f g = fmap (either (fmap Left) (fmap Right)) (f PP.+++! g)

-- {

-- Boilerplate instance definitions.  There's no choice here apart
-- from the order in which the applicative is applied.

instance Functor (PackMap a b s) where
  fmap f (PackMap g) = PackMap ((fmap . fmap . fmap) f g)

instance Applicative (PackMap a b s) where
  pure x = PackMap (pure (pure (pure x)))
  PackMap f <*> PackMap x = PackMap (liftA2 (liftA2 (<*>)) f x)

instance Profunctor (PackMap a b) where
  dimap f g (PackMap q) = PackMap (fmap (dimap f (fmap g)) q)

instance ProductProfunctor (PackMap a b) where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor (PackMap a b) where
  f +++! g = (PackMap (\x -> eitherFunction (f' x) (g' x)))
    where PackMap f' = f
          PackMap g' = g

-- }
