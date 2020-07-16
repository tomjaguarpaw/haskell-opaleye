{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE Rank2Types #-}

module Rectangular where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

-- Rows of type r, along with a profunctor that has been prepared for
-- rows of type r.  The rows also come with an extra argument a, which
-- remains polymorphic so stuff can be smuggled in an out.
data W a p c where
  W :: p r c -> NEL.NonEmpty (a, r) -> W a p c

instance P.Profunctor p => Functor (W a p) where
  fmap f (W p l) = W (P.rmap f p) l

newtype U p b c where
  U :: (forall a. NEL.NonEmpty (a, b) -> Maybe (W a p c)) -> U p b c

pureU :: p b c -> U p b c
pureU p = U (\l -> Just (W p l))

instance P.Profunctor p => P.Profunctor (U p) where
  dimap f g (U h) = U (P.dimap ((fmap . fmap) f) ((fmap . fmap) g) h)

instance PP.ProductProfunctor p
  => PP.ProductProfunctor (U p) where
  purePP = pureU . PP.purePP

  -- Probably ought to be more lazy in the arguments here too
  U f **** U g = U (\l -> do
    let l' = fmap (\(a, b) -> ((a, b), b)) l
    wf <- f l'
    case wf of
      W pf fl -> do
        let l'' = fmap (\((a, b), r) -> ((a, r), b)) fl
        wg <- g l''
        case wg of
          W pg gl ->
            pure $
            W (P.lmap fst pf  PP.**** P.lmap snd pg)
              (fmap (\((a, r), r') -> (a, (r, r'))) gl))

instance P.Profunctor p => PP.SumProfunctor (U p) where
  f +++! g = U (\l -> do
    let eithers :: NEL.NonEmpty (a, Either b b')
                -> Maybe (Either (NEL.NonEmpty (a, b))
                                 (NEL.NonEmpty (a, b')))
        eithers ll = case NEL.uncons ll of
          ((a, ebb), Nothing) -> Just $ case ebb of
            Left b   -> Left  (pure (a, b))
            Right b' -> Right (pure (a, b'))
          ((a, ebb), Just a_ebbs) -> case eithers a_ebbs of
            Nothing  -> Nothing
            Just eab_ab -> case (ebb, eab_ab) of
              (Left b,   Left abs_)  -> Just (Left  ((a, b)  NEL.<| abs_))
              (Right b', Right ab's) -> Just (Right ((a, b') NEL.<| ab's))
              _ -> Nothing

    e_abs_ab's <- eithers l

    case e_abs_ab's of
      Left abs_  -> case f of U f' -> (fmap . fmap) Left  (f' abs_)
      Right ab's -> case g of U g' -> (fmap . fmap) Right (g' ab's)
    )
