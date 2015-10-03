{-# LANGUAGE FlexibleContexts #-}

module Opaleye.Operators (module Opaleye.Operators,
                          (O..&&)) where

import qualified Data.Foldable as F

import           Opaleye.Internal.Column (Column(Column), unsafeCase_,
                                          unsafeIfThenElse, unsafeGt)
import qualified Opaleye.Internal.Column as C
import           Opaleye.Internal.QueryArr (QueryArr(QueryArr))
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Operators as O
import           Opaleye.Internal.Helpers   ((.:))
import qualified Opaleye.Order as Ord
import qualified Opaleye.PGTypes as T

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

{-| Restrict query results to a particular condition.  Corresponds to
    the guard method of the MonadPlus class.
-}
restrict :: QueryArr (Column T.PGBool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.restrict predicate primQ, t0)

doubleOfInt :: Column T.PGInt4 -> Column T.PGFloat8
doubleOfInt (Column e) = Column (HPQ.CastExpr "float8" e)

infix 4 .==
(.==) :: Column a -> Column a -> Column T.PGBool
(.==) = C.binOp HPQ.OpEq

infix 4 ./=
(./=) :: Column a -> Column a -> Column T.PGBool
(./=) = C.binOp HPQ.OpNotEq

infix 4 .===
-- | A polymorphic equality operator that works for all types that you
-- have run `makeAdaptorAndInstance` on.  This may be unified with
-- `.==` in a future version.
(.===) :: D.Default O.EqPP columns columns => columns -> columns -> Column T.PGBool
(.===) = (O..==)

infix 4 ./==
-- | A polymorphic inequality operator that works for all types that
-- you have run `makeAdaptorAndInstance` on.  This may be unified with
-- `.==` in a future version.
(./==) :: D.Default O.EqPP columns columns => columns -> columns -> Column T.PGBool
(./==) = Opaleye.Operators.not .: (O..==)

infix 4 .>
(.>) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.>) = unsafeGt

infix 4 .<
(.<) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.<) = C.binOp HPQ.OpLt

infix 4 .<=
(.<=) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.<=) = C.binOp HPQ.OpLtEq

infix 4 .>=
(.>=) :: Ord.PGOrd a => Column a -> Column a -> Column T.PGBool
(.>=) = C.binOp HPQ.OpGtEq

case_ :: [(Column T.PGBool, Column a)] -> Column a -> Column a
case_ = unsafeCase_

ifThenElse :: Column T.PGBool -> Column a -> Column a -> Column a
ifThenElse = unsafeIfThenElse

infixr 2 .||
(.||) :: Column T.PGBool -> Column T.PGBool -> Column T.PGBool
(.||) = C.binOp HPQ.OpOr

not :: Column T.PGBool -> Column T.PGBool
not = C.unOp HPQ.OpNot

(.++) :: Column T.PGText -> Column T.PGText -> Column T.PGText
(.++) = C.binOp HPQ.OpCat

lower :: Column T.PGText -> Column T.PGText
lower = C.unOp HPQ.OpLower

upper :: Column T.PGText -> Column T.PGText
upper = C.unOp HPQ.OpUpper

like :: Column T.PGText -> Column T.PGText -> Column T.PGBool
like = C.binOp HPQ.OpLike

ors :: F.Foldable f => f (Column T.PGBool) -> Column T.PGBool
ors = F.foldl' (.||) (T.pgBool False)

in_ :: (Functor f, F.Foldable f) => f (Column a) -> Column a -> Column T.PGBool
in_ hs w = ors . fmap (w .==) $ hs
