{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.Operators where

import           Opaleye.Internal.Column (Column)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.QueryArr as QA
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Table as Table
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.PGTypes as T

import           Data.Profunctor (Profunctor, dimap, lmap, rmap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product.Default as D

infix 4 .==
(.==) :: forall columns. D.Default EqPP columns columns
      => columns -> columns -> Column T.PGBool
(.==) = eqExplicit (D.def :: EqPP columns columns)

infixr 3 .&&
(.&&) :: Column T.PGBool -> Column T.PGBool -> Column T.PGBool
(.&&) = C.binOp HPQ.OpAnd

-- Probably should be newtype
data EqPP a b = EqPP (a -> a -> Column T.PGBool)

eqExplicit :: EqPP columns a -> columns -> columns -> Column T.PGBool
eqExplicit (EqPP f) = f

instance D.Default EqPP (Column a) (Column a) where
  def = EqPP C.unsafeEq


data RelExprMaker a b =
  forall c. RelExprMaker {
      relExprVCM :: TM.ViewColumnMaker a c
    , relExprCM  :: TM.ColumnMaker c b
    }

relExprColumn :: RelExprMaker String (Column a)
relExprColumn = RelExprMaker TM.tableColumn TM.column

instance D.Default RelExprMaker String (Column a) where
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
  QA.simpleQueryArr $ \(a, tag) ->
    let (primExprs, projcols) = runRelExprMaker rem_ tag strings
        primQ :: PQ.PrimQuery
        primQ = PQ.RelExpr (pe a) projcols
    in (primExprs, primQ, Tag.next tag)

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
-- }
