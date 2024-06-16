-- | Left, right, and full outer joins.

{-# LANGUAGE TypeFamilies          #-}

module Opaleye.Join where

import qualified Opaleye.Field               as F
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Join as J
import qualified Opaleye.Internal.MaybeFields as M
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Select   as S
import qualified Opaleye.SqlTypes as T

import qualified Data.Profunctor.Product.Default as D

-- * The recommended way of performing joins in Opaleye

-- $ref
--
-- Opaleye supports inner joins, left/right joins and full outer
-- joins.  Instead of using them directly we recommend the following,
-- which provide APIs that are more familiar to a Haskell programmer
-- and more composable:
--
-- - Inner joins: use 'Opaleye.Operators.where_' directly, along with
--   @do@ notation (or use 'Opaleye.Operators.restrict' directly,
--   along with arrow notation)
--
-- - Left/right joins: use 'optional'
--
-- We suspect the following do not have real world use cases.  If you
-- have one then we'd love to hear about it. Please [open a new issue
-- on the Opaleye
-- project](http://github.com/tomjaguarpaw/haskell-opaleye/issues/new)
-- and tell us about it.
--
-- - Left/right joins which really must not use @LATERAL@: use 'optionalRestrict'
--
-- - Full outer joins: use 'Opaleye.FunctionalJoin.fullJoinF'

-- | Convenient access to lateral left/right join
-- functionality. Performs a @LATERAL LEFT JOIN@ under the hood and
-- has behaviour equivalent to the following Haskell function:
--
-- @
-- optional :: [a] -> [Maybe a]
-- optional q = case q of
--     [] -> [Nothing]
--     xs -> map Just xs
-- @
--
-- That is, if @q :: 'SelectArr' i a@ returns no rows, @'optional' q
-- :: 'SelectArr' i ('MaybeFields' a)@ returns exactly one \"Nothing\"
-- row.  Otherwise, @'optional' q@ returns exactly the rows of @q@
-- wrapped in \"Just\".  For example,
--
-- @
-- > let l1 = ["one", "two", "three"] :: [Field SqlText]
-- > 'Opaleye.RunSelect.runSelectI' conn ('optional' ('Opaleye.Values.values' l1))
-- [Just "one", Just "two", Just "three"]
--
-- > let l2 = [] :: [Field SqlText]
-- > 'Opaleye.RunSelect.runSelectI' conn ('optional' ('Opaleye.Values.values' l2))
-- [Nothing]
-- @
--
-- 'optionalRestrict' is a special case of @optional@ and could be
-- written in terms of @optional@ as follows (except that
-- 'optionalRestrict' doesn't use @LATERAL@ under the hood and
-- @optional@ does).
--
-- @
-- optionalRestrict q = optional $ proc cond -> do
--   a <- q -< ()
--   restrict -< cond a
--   returnA -< a
-- @
optional :: D.Default U.Unpackspec a a
         => S.SelectArr i a
         -- ^ Input query
         -> S.SelectArr i (M.MaybeFields a)
         -- ^ The rows of the input query wrapped in \"Just\", unless
         -- the input query has no rows in which case a single row of
         -- \"Nothing\"
optional = M.optional

-- | Convenient access to left/right join functionality.  Performs a
-- @LEFT JOIN@ under the hood and has behaviour equivalent to the
-- following Haskell function:
--
-- @
-- optionalRestrict :: [a] -> (a -> Bool) -> [Maybe a]
-- optionalRestrict xs p =
--    case filter p xs of []  -> [Nothing]
--                        xs' -> map Just xs'
-- @
--
-- For example,
--
-- @
-- > let l = [1, 10, 100, 1000] :: [Field SqlInt4]
-- > 'Opaleye.RunSelect.runSelectI' conn (proc () -> optionalRestrict ('Opaleye.Values.values' l) -\< (.> 100000))
-- [Nothing]
--
-- > 'Opaleye.RunSelect.runSelectI' conn (proc () -> optionalRestrict ('Opaleye.Values.values' l) -\< (.> 15))
-- [Just 100,Just 1000]
-- @
--
-- See the documentation of 'leftJoin' for how to use
-- 'optionalRestrict' to replace 'leftJoin' (and by symmetry,
-- 'rightJoin').
optionalRestrict :: D.Default U.Unpackspec a a
                 => S.Select a
                 -- ^ Input query
                 -> S.SelectArr (a -> F.Field T.SqlBool) (M.MaybeFields a)
                 -- ^ If any rows of the input query satisfy the
                 -- condition then return them (wrapped in \"Just\").
                 -- If none of them satisfy the condition then return a
                 -- single row of \"Nothing\"
optionalRestrict = J.optionalRestrict

-- * Direct access to joins (not recommended)

-- $ref2
--
-- You probably want use the alternatives listed at the top of this
-- module instead of these.
-- The use of the @'D.Default' 'NullMaker'@ typeclass means that the compiler will
-- have trouble inferring types.  It is strongly recommended that you
-- provide full type signatures when using the join functions.
-- Example specialization:
--
-- @
-- leftJoin :: Select (Field a, Field b)
--          -> Select (Field c, FieldNullable d)
--          -> (((Field a, Field b), (Field c, FieldNullable d)) -> Field 'Opaleye.SqlTypes.SqlBool')
--          -> Select ((Field a, Field b), (FieldNullable c, FieldNullable d))
-- @

-- | We suggest you use 'optional' instead.  Instead of writing
-- \"@'Opaleye.Join.leftJoin' qL qR cond@\" you can write
--
-- @
-- do
--   fieldsL <- qL
--   maybeFieldsR \<- 'optional' $ do
--     fieldsR <- qR
--     cond (fieldsL, fieldsR)
--     pure fieldsR
--   pure (fieldsL, maybeFieldsR)
-- @
--
-- Typically everything except the 'optional' block can be inlined in
-- surrounding @do@ notation.  In such cases, readability and
-- maintainability increase dramatically.
--
-- Alternatively, if you have a reason to avoid @LATERAL@ joins you
-- can use 'optionalRestrict' and arrow notation.
--
-- @
-- proc () -> do
--   fieldsL <- qL -< ()
--   maybeFieldsR \<- 'optionalRestrict' qR -\< 'Prelude.curry' cond fieldsL
--   'Control.Arrow.returnA' -< (fieldsL, maybeFieldsR)
-- @
leftJoin  :: (D.Default U.Unpackspec fieldsL fieldsL,
              D.Default U.Unpackspec fieldsR fieldsR,
              D.Default J.NullMaker fieldsR nullableFieldsR)
          => S.Select fieldsL  -- ^ Left query
          -> S.Select fieldsR  -- ^ Right query
          -> ((fieldsL, fieldsR) -> F.Field T.SqlBool) -- ^ Condition on which to join
          -> S.Select (fieldsL, nullableFieldsR) -- ^ Left join
leftJoin = leftJoinExplicit D.def D.def D.def

-- | We suggest you don't use this.  'optional' or 'optionalRestrict' are probably
-- better for your use case.  'Opaleye.Join.leftJoinA' is the same as
-- 'optionalRestrict' except without the return type wrapped in
-- 'Opaleye.Internal.MaybeFields.MaybeFields'.

leftJoinA :: (D.Default U.Unpackspec fieldsR fieldsR,
              D.Default J.NullMaker fieldsR nullableFieldsR)
          => S.Select fieldsR
          -- ^ Right query
          -> S.SelectArr (fieldsR -> F.Field T.SqlBool) nullableFieldsR
          -- ^ Condition on which to join goes in, left join
          -- result comes out
leftJoinA = leftJoinAExplict D.def D.def

-- | We suggest you use 'optional' or 'optionalRestrict' instead.  See 'leftJoin'
-- for more details.
rightJoin  :: (D.Default U.Unpackspec fieldsL fieldsL,
               D.Default U.Unpackspec fieldsR fieldsR,
               D.Default J.NullMaker fieldsL nullableFieldsL)
           => S.Select fieldsL -- ^ Left query
           -> S.Select fieldsR -- ^ Right query
           -> ((fieldsL, fieldsR) -> F.Field T.SqlBool) -- ^ Condition on which to join
           -> S.Select (nullableFieldsL, fieldsR) -- ^ Right join
rightJoin = rightJoinExplicit D.def D.def D.def


fullJoin  :: (D.Default U.Unpackspec fieldsL fieldsL,
              D.Default U.Unpackspec fieldsR fieldsR,
              D.Default J.NullMaker fieldsL nullableFieldsL,
              D.Default J.NullMaker fieldsR nullableFieldsR)
          => S.Select fieldsL -- ^ Left query
          -> S.Select fieldsR -- ^ Right query
          -> ((fieldsL, fieldsR) -> F.Field T.SqlBool) -- ^ Condition on which to join
          -> S.Select (nullableFieldsL, nullableFieldsR) -- ^ Full outer join
fullJoin = fullJoinExplicit D.def D.def D.def D.def

-- * Explicit versions

leftJoinExplicit :: U.Unpackspec fieldsL fieldsL
                 -> U.Unpackspec fieldsR fieldsR
                 -> J.NullMaker fieldsR nullableFieldsR
                 -> S.Select fieldsL -> S.Select fieldsR
                 -> ((fieldsL, fieldsR) -> F.Field T.SqlBool)
                 -> S.Select (fieldsL, nullableFieldsR)
leftJoinExplicit uA uB nullmaker =
  J.joinExplicit uA uB id (J.toNullable nullmaker) PQ.LeftJoin

leftJoinAExplict :: U.Unpackspec fieldsR fieldsR
                 -> J.NullMaker fieldsR nullableFieldsR
                 -> S.Select fieldsR
                 -> S.SelectArr (fieldsR -> F.Field T.SqlBool) nullableFieldsR
leftJoinAExplict = J.leftJoinAExplicit

rightJoinExplicit :: U.Unpackspec fieldsL fieldsL
                  -> U.Unpackspec fieldsR fieldsR
                  -> J.NullMaker fieldsL nullableFieldsL
                  -> S.Select fieldsL -> S.Select fieldsR
                  -> ((fieldsL, fieldsR) -> F.Field T.SqlBool)
                  -> S.Select (nullableFieldsL, fieldsR)
rightJoinExplicit uA uB nullmaker =
  J.joinExplicit uA uB (J.toNullable nullmaker) id PQ.RightJoin


fullJoinExplicit :: U.Unpackspec fieldsL fieldsL
                 -> U.Unpackspec fieldsR fieldsR
                 -> J.NullMaker fieldsL nullableFieldsL
                 -> J.NullMaker fieldsR nullableFieldsR
                 -> S.Select fieldsL -> S.Select fieldsR
                 -> ((fieldsL, fieldsR) -> F.Field T.SqlBool)
                 -> S.Select (nullableFieldsL, nullableFieldsR)
fullJoinExplicit uA uB nullmakerA nullmakerB =
  J.joinExplicit uA uB (J.toNullable nullmakerA) (J.toNullable nullmakerB) PQ.FullJoin

optionalRestrictExplicit :: U.Unpackspec a a
                         -> S.Select a
                         -> S.SelectArr (a -> F.Field T.SqlBool) (M.MaybeFields a)
optionalRestrictExplicit = J.optionalRestrictExplicit

-- The Unpackpec is not used but I'm adding it in case we discover we
-- need it in the future.  Then we can use it without breaking the
-- API.
optionalExplicit :: U.Unpackspec a a
                 -> S.SelectArr i a
                 -> S.SelectArr i (M.MaybeFields a)
optionalExplicit _ = M.optional
