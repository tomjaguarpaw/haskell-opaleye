{-# OPTIONS_HADDOCK not-home #-}

module Opaleye.Internal.Order where

import           Data.Function                        (on)
import qualified Data.Functor.Contravariant           as C
import qualified Data.Functor.Contravariant.Divisible as Divisible
import qualified Data.List.NonEmpty                   as NL
import qualified Data.Monoid                          as M
import qualified Data.Profunctor                      as P
import qualified Data.Semigroup                       as S
import qualified Data.Void                            as Void
import qualified Opaleye.Field                        as F
import qualified Opaleye.Internal.Column              as IC
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PrimQuery           as PQ
import qualified Opaleye.Internal.Tag                 as T
import qualified Opaleye.Internal.Unpackspec          as U

{-|
An `Order` @a@ represents a sort order and direction for the elements
of the type @a@. Multiple `Order`s can be composed with
`Data.Monoid.mappend` or @(\<\>)@ from "Data.Monoid".  If two rows are
equal according to the first `Order` in the @mappend@, the second is
used, and so on.
-}

-- Like the (columns -> RowParser haskells) field of FromFields this
-- type is "too big".  We never actually look at the 'a' (in the
-- FromFields case the 'columns') except to check the "structure".
-- This is so we can support a SumProfunctor instance.
newtype Order a = Order (a -> [(HPQ.OrderOp, HPQ.PrimExpr)])

instance C.Contravariant Order where
  contramap f (Order g) = Order (P.lmap f g)

instance S.Semigroup (Order a) where
  Order o <> Order o' = Order (o S.<> o')

instance M.Monoid (Order a) where
  mempty = Order M.mempty
  mappend = (S.<>)

instance Divisible.Divisible Order where
  divide f o o' = M.mappend (C.contramap (fst . f) o)
                            (C.contramap (snd . f) o')
  conquer = M.mempty

instance Divisible.Decidable Order where
  lose f = C.contramap f (Order Void.absurd)
  choose f (Order o) (Order o') = C.contramap f (Order (either o o'))

order :: HPQ.OrderOp -> (a -> F.Field_ n b) -> Order a
order op f = Order (fmap (\column -> [(op, IC.unColumn column)]) f)

orderByU :: Order a -> (a, PQ.PrimQuery) -> (a, PQ.PrimQuery)
orderByU os (columns, primQ) = (columns, primQ')
  where primQ' = PQ.DistinctOnOrderBy Nothing oExprs primQ
        oExprs = orderExprs columns os

orderExprs :: a -> Order a -> [HPQ.OrderExpr]
orderExprs x (Order os) = map (uncurry HPQ.OrderExpr) (os x)

limit' :: Int -> (a, PQ.PrimQuery) -> (a, PQ.PrimQuery)
limit' n (x, q) = (x, PQ.Limit (PQ.LimitOp n) q)

offset' :: Int -> (a, PQ.PrimQuery) -> (a, PQ.PrimQuery)
offset' n (x, q) = (x, PQ.Limit (PQ.OffsetOp n) q)

distinctOn :: U.Unpackspec b b -> (a -> b)
           -> (a, PQ.PrimQuery) -> (a, PQ.PrimQuery)
distinctOn ups proj = distinctOnBy ups proj M.mempty

distinctOnBy :: U.Unpackspec b b -> (a -> b) -> Order a
             -> (a, PQ.PrimQuery) -> (a, PQ.PrimQuery)
distinctOnBy ups proj ord (cols, pq) = (cols, pqOut)
    where pqOut = case NL.nonEmpty (U.collectPEs ups (proj cols)) of
            Just xs -> PQ.DistinctOnOrderBy (Just xs) oexprs pq
            Nothing -> PQ.Limit (PQ.LimitOp 1) (PQ.DistinctOnOrderBy Nothing oexprs pq)
          oexprs = orderExprs cols ord

-- | Order the results of a given query exactly, as determined by the given list
-- of input fields. Note that this list does not have to contain an entry for
-- every result in your query: you may exactly order only a subset of results,
-- if you wish. Rows that are not ordered according to the input list are
-- returned /after/ the ordered results, in the usual order the database would
-- return them (e.g. sorted by primary key). Exactly-ordered results always come
-- first in a result set. Entries in the input list that are /not/ present in
-- result of a query are ignored.
exact :: [IC.Field_ n b] -> (a -> IC.Field_ n b) -> Order a
exact xs k = maybe M.mempty go (NL.nonEmpty xs) where
  -- Create an equality AST node, between two columns, essentially
  -- stating "(column = value)" syntactically.
  mkEq  = HPQ.BinExpr (HPQ.:=) `on` IC.unColumn

  -- The AST operation: ORDER BY (equalities...) DESC NULLS FIRST
  -- NOTA BENE: DESC is mandatory (otherwise the result is reversed, as you are
  -- "descending" down the list of equalities from the front, rather than
  -- "ascending" from the end of the list.) NULLS FIRST strictly isn't needed;
  -- but HPQ.OrderOp currently mandates a value for both the direction
  -- (OrderDirection) and the rules for null (OrderNulls) values, in the
  -- OrderOp constructor.
  astOp = HPQ.OrderOp HPQ.OpDesc HPQ.NullsFirst

  -- Final result: ORDER BY (equalities...) DESC NULLS FIRST, with a given
  -- list of equality operations, created via 'mkEq'
  go givenOrder = Order $ flip fmap k $ \col ->
    [(astOp, HPQ.ListExpr $ NL.map (mkEq col) givenOrder)]
