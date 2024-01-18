{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.Operators where

import Control.Applicative (liftA2)

import           Opaleye.Internal.Column (Field_(Column))
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.QueryArr as QA
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.PGTypesExternal as T
import qualified Opaleye.Field as F
import           Opaleye.Field (Field)
import qualified Opaleye.Select as S

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product.Default as D

restrict :: S.SelectArr (F.Field T.SqlBool) ()
restrict = QA.selectArr f where
  -- A where clause can always refer to columns defined by the query
  -- it references so needs no special treatment on LATERAL.
  f = pure (\(Column predicate) -> ((), PQ.aRestrict predicate))

infix 4 .==
(.==) :: forall columns. D.Default EqPP columns columns
      => columns -> columns -> Field T.PGBool
(.==) = eqExplicit (D.def :: EqPP columns columns)

infixr 2 .||

(.||) :: F.Field T.SqlBool -> F.Field T.SqlBool -> F.Field T.SqlBool
(.||) = C.binOp HPQ.OpOr

infixr 3 .&&

-- | Boolean and
(.&&) :: Field T.PGBool -> Field T.PGBool -> Field T.PGBool
(.&&) = C.binOp HPQ.OpAnd

not :: F.Field T.SqlBool -> F.Field T.SqlBool
not = C.unOp HPQ.OpNot

newtype EqPP a b = EqPP (a -> a -> Field T.PGBool)

eqPPField :: EqPP (Field a) ignored
eqPPField = EqPP C.unsafeEq

eqExplicit :: EqPP columns a -> columns -> columns -> Field T.PGBool
eqExplicit (EqPP f) = f

instance D.Default EqPP (Field a) (Field a) where
  def = eqPPField


newtype IfPP a b = IfPP (Field T.PGBool -> a -> a -> b)

ifExplict :: IfPP columns columns'
          -> Field T.PGBool
          -> columns
          -> columns
          -> columns'
ifExplict (IfPP f) = f

ifPPField :: IfPP (Field_ n a) (Field_ n a)
ifPPField = D.def

instance D.Default IfPP (Field_ n a) (Field_ n a) where
  def = IfPP C.unsafeIfThenElse


newtype RelExprPP a b = RelExprPP (Tag.Tag -> PM.PM [HPQ.Symbol] b)


runRelExprPP :: RelExprPP a b -> Tag.Tag -> (b, [HPQ.Symbol])
runRelExprPP (RelExprPP m) = PM.run . m


instance D.Default RelExprPP (Field_ n a) (Field_ n a) where
  def = relExprColumn


relExprColumn :: RelExprPP (Field_ n a) (Field_ n a)
relExprColumn = RelExprPP $ fmap Column . PM.extract "relExpr"


relationValuedExprExplicit :: RelExprPP columns columns
                           -> (a -> HPQ.PrimExpr)
                           -> QA.QueryArr a columns
relationValuedExprExplicit relExprPP pe =
  QA.productQueryArr' $ do
    (columns, symbols) <- runRelExprPP relExprPP <$> Tag.fresh
    pure $ \a -> (columns, PQ.RelExpr (pe a) symbols)


relationValuedExpr :: D.Default RelExprPP columns columns
                   => (a -> HPQ.PrimExpr)
                   -> QA.QueryArr a columns
relationValuedExpr = relationValuedExprExplicit D.def

-- { Boilerplate instances

instance Profunctor EqPP where
  dimap f _ (EqPP h) = EqPP (\a a' -> h (f a) (f a'))

instance ProductProfunctor EqPP where
  empty = EqPP (\() () -> T.pgBool True)
  EqPP f ***! EqPP f' = EqPP (\a a' ->
                               f (fst a) (fst a') .&& f' (snd a) (snd a'))

instance Profunctor RelExprPP where
  dimap _ f (RelExprPP m) = RelExprPP (fmap (fmap f) m)

instance ProductProfunctor RelExprPP where
  empty = RelExprPP (pure (pure ()))
  RelExprPP f ***! RelExprPP g =
    RelExprPP $ liftA2 (liftA2 (,)) f g

instance Profunctor IfPP where
  dimap f g (IfPP h) = IfPP (\b a a' -> g (h b (f a) (f a')))

instance ProductProfunctor IfPP where
  empty = IfPP (\_ () () -> ())
  IfPP f ***! IfPP f' = IfPP (\b a a1 ->
                               (f b (fst a) (fst a1), f' b (snd a) (snd a1)))

-- }
