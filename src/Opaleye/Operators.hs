{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Operators on 'Column's.  Numeric 'Column' types are instances of
-- 'Num', so you can use '*', '/', '+', '-' on them.

module Opaleye.Operators (module Opaleye.Operators,
                          (O..&&)) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F

import           Opaleye.Internal.Column (Column(Column), unsafeCase_,
                                          unsafeIfThenElse, unsafeGt)
import qualified Opaleye.Internal.Column as C
import           Opaleye.Internal.QueryArr (QueryArr(QueryArr), Query)
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Operators as O
import           Opaleye.Internal.Helpers   ((.:))
import qualified Opaleye.Order as Ord
import qualified Opaleye.PGTypes as T

import qualified Opaleye.Column   as Column
import qualified Opaleye.Distinct as Distinct
import qualified Opaleye.Join     as Join

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Profunctor.Product.Default as D

{-| Restrict query results to a particular condition.  Corresponds to
the guard method of the MonadPlus class.  You would typically use
'restrict' if you want to use 'A.Arrow' notation.  -}
restrict :: QueryArr (Column T.PGBool) ()
restrict = QueryArr f where
  f (Column predicate, primQ, t0) = ((), PQ.restrict predicate primQ, t0)

{-| Filter a 'QueryArr' to only those rows where the given condition
holds.  This is the 'QueryArr' equivalent of 'Prelude.filter' from the
'Prelude'.  You would typically use 'keepWhen' if you want to use a
"point free" style.-}
keepWhen :: (a -> Column T.PGBool) -> QueryArr a a
keepWhen p = proc a -> do
  restrict  -< p a
  A.returnA -< a

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

-- | True when any element of the container is true
ors :: F.Foldable f => f (Column T.PGBool) -> Column T.PGBool
ors = F.foldl' (.||) (T.pgBool False)

in_ :: (Functor f, F.Foldable f) => f (Column a) -> Column a -> Column T.PGBool
in_ hs w = ors . fmap (w .==) $ hs

-- | True if the first argument occurs amongst the rows of the second,
-- false otherwise.
--
-- This operation is equivalent to Postgres's @IN@ operator but, for
-- expediency, is currently implemented using a @LEFT JOIN@.  Please
-- file a bug if this causes any issues in practice.
inQuery :: D.Default O.EqPP columns columns
        => columns -> QueryArr () columns -> Query (Column T.PGBool)
inQuery c q = qj'
  where q' = proc () -> do
          x' <- q -< ()
          restrict -< c .=== x'
          A.returnA -< 1

        qd :: Query (Column T.PGInt4)
        qd = Distinct.distinct q'

        qj :: Query ((Column T.PGInt4, Column T.PGBool),
                     Column (C.Nullable T.PGInt4))
        qj = Join.leftJoin (A.arr (const (1, T.pgBool True))) qd
                           (\((y, _), x) -> x .== y)

        qj' :: Query (Column T.PGBool)
        qj' = proc () -> do
          ((_, _), d) <- qj -< ()
          A.returnA -< Opaleye.Operators.not (Column.isNull d)
