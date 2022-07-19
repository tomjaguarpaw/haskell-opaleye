{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.Operators where

import           Opaleye.Internal.Column (Field_(Column))
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.QueryArr as QA
import qualified Opaleye.Internal.Table as Table
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.PGTypesExternal as T
import qualified Opaleye.Field as F
import           Opaleye.Field (Field)
import qualified Opaleye.Select as S

import           Data.Profunctor (Profunctor, dimap, lmap, rmap)
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


-- This seems to be the only place we use ViewColumnMaker now.
data RelExprMaker a b =
  forall c. RelExprMaker {
      relExprVCM :: TM.ViewColumnMaker a c
    , relExprCM  :: U.Unpackspec c b
    }

relExprColumn :: RelExprMaker String (Field_ n a)
relExprColumn = RelExprMaker TM.tableColumn U.unpackspecField

instance D.Default RelExprMaker String (Field_ n a) where
  def = relExprColumn

runRelExprMaker :: RelExprMaker strings columns
                -> Tag.Tag
                -> strings
                -> (columns, [(HPQ.Symbol, HPQ.PrimExpr)])
runRelExprMaker rem_ tag =
  case rem_ of RelExprMaker vcm cm -> Table.runColumnMaker cm tag
                                    . TM.runViewColumnMaker vcm

relationValuedExprExplicit :: RelExprMaker strings columns
                           -> strings
                           -> (a -> HPQ.PrimExpr)
                           -> QA.QueryArr a columns
relationValuedExprExplicit rem_ strings pe =
  QA.productQueryArr' $ \a -> do
    tag <- Tag.fresh
    let (primExprs, projcols) = runRelExprMaker rem_ tag strings
        primQ :: PQ.PrimQuery
        primQ = PQ.RelExpr (pe a) projcols
    pure (primExprs, primQ)

relationValuedExpr :: D.Default RelExprMaker strings columns
                   => strings
                   -> (a -> HPQ.PrimExpr)
                   -> QA.QueryArr a columns
relationValuedExpr = relationValuedExprExplicit D.def

-- { Boilerplate instances

instance Profunctor EqPP where
  dimap f _ (EqPP h) = EqPP (\a a' -> h (f a) (f a'))

instance ProductProfunctor EqPP where
  empty = EqPP (\() () -> T.pgBool True)
  EqPP f ***! EqPP f' = EqPP (\a a' ->
                               f (fst a) (fst a') .&& f' (snd a) (snd a'))

instance Profunctor RelExprMaker where
  dimap f g (RelExprMaker a b) = RelExprMaker (lmap f a) (rmap g b)

instance ProductProfunctor RelExprMaker where
  empty = RelExprMaker empty empty
  f ***! g = case f of RelExprMaker vcmf cmf ->
                        case g of RelExprMaker vcmg cmg ->
                                    h vcmf vcmg cmf cmg
    where h vcmg vcmf cmg cmf = RelExprMaker (vcmg ***! vcmf)
                                             (cmg  ***! cmf)

instance Profunctor IfPP where
  dimap f g (IfPP h) = IfPP (\b a a' -> g (h b (f a) (f a')))

instance ProductProfunctor IfPP where
  empty = IfPP (\_ () () -> ())
  IfPP f ***! IfPP f' = IfPP (\b a a1 ->
                               (f b (fst a) (fst a1), f' b (snd a) (snd a1)))

-- }
