{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opaleye.Internal.Operators where

import qualified Opaleye.Internal.Binary as B
import           Opaleye.Internal.Column (Column)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.QueryArr as QA
import qualified Opaleye.Internal.Table as Table
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.PGTypes as T

import qualified Data.Monoid as M
import qualified Data.Functor.Constant as Constant
import           Data.Profunctor (Profunctor, dimap, lmap, rmap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product.Default as D
import           Data.Semigroup (Semigroup, (<>))

infix 4 .==
(.==) :: forall columns. D.Default EqPP columns columns
      => columns -> columns -> Column T.PGBool
(.==) = eqExplicit (D.def :: EqPP columns columns)

infixr 3 .&&

-- | Boolean and
(.&&) :: Column T.PGBool -> Column T.PGBool -> Column T.PGBool
(.&&) = C.binOp HPQ.OpAnd

newtype PGBoolAnd = PGBoolAnd { unPGBoolAnd :: Column T.PGBool }

instance Semigroup PGBoolAnd where
  PGBoolAnd x <> PGBoolAnd y = PGBoolAnd (x .&& y)

instance M.Monoid PGBoolAnd where
  mempty = PGBoolAnd (T.pgBool True)
  mappend = (<>)

type EqPP = B.Binaryspec

eqExplicit :: EqPP columns a -> columns -> columns -> Column T.PGBool
eqExplicit eqpp =
  curry (unPGBoolAnd
  . Constant.getConstant
  . B.runBinaryspec eqpp
               (\(a, b)
                -> Constant.Constant
                   (PGBoolAnd
                    (C.unsafeEq (C.Column a) (C.Column b)))))


type IfPP = EqPP

ifExplict :: IfPP columns columns'
          -> Column T.PGBool
          -> columns
          -> columns
          -> columns'
ifExplict eqpp b x y =
  curry (B.runBinaryspec eqpp
             (\(x', y') b' -> C.unColumn
                              (C.unsafeIfThenElse b'
                               (C.Column x') (C.Column y'))))
             x y b

-- This seems to be the only place we use ViewColumnMaker now.
data RelExprMaker a b =
  forall c. RelExprMaker {
      relExprVCM :: TM.ViewColumnMaker a c
    , relExprCM  :: U.Unpackspec c b
    }

relExprColumn :: RelExprMaker String (Column a)
relExprColumn = RelExprMaker TM.tableColumn U.unpackspecColumn

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
