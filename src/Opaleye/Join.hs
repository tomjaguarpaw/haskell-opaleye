{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- | Left, right, and full outer joins.
--
-- "Opaleye.FunctionalJoin" provides a much nicer, Haskelly, interface
-- to joins than this module, which sticks to the (horrible) standard
-- \"make missing rows NULL\" interface that SQL provides.
--
-- If you want inner joins, just use 'restrict' instead.
--
-- The use of the 'D.Default' typeclass means that the compiler will
-- have trouble inferring types.  It is strongly recommended that you
-- provide full type signatures when using the join functions.
--
-- Example specialization:
--
-- @
-- leftJoin :: Select (Field a, Field b)
--          -> Select (Field c, FieldNullable d)
--          -> (((Field a, Field b), (Field c, FieldNullable d)) -> Field 'Opaleye.SqlTypes.SqlBool')
--          -> Select ((Field a, Field b), (FieldNullable c, FieldNullable d))
-- @

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

--{-# LANGUAGE PolyKinds #-}
--  ^ Would rather not have this.
--    Can I add a kind signature instead?

module Opaleye.Join where

import qualified Opaleye.Field               as F
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Join as J
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Map as Map
import qualified Opaleye.Select   as S
import qualified Opaleye.SqlTypes as T

import qualified Opaleye.Internal.TypeFamilies2 as TF2
import           Opaleye.Column

import qualified Data.Profunctor.Product.Default as D

-- * Joins

leftJoin  :: (D.Default U.Unpackspec fieldsL fieldsL,
              D.Default U.Unpackspec fieldsR fieldsR,
              D.Default J.NullMaker fieldsR nullableFieldsR)
          => S.Select fieldsL  -- ^ Left query
          -> S.Select fieldsR  -- ^ Right query
          -> ((fieldsL, fieldsR) -> F.Field T.SqlBool) -- ^ Condition on which to join
          -> S.Select (fieldsL, nullableFieldsR) -- ^ Left join
leftJoin = leftJoinExplicit D.def D.def D.def

-- | 'leftJoinA' is a convenient way of using left joins within arrow
-- notation
leftJoinA :: (D.Default U.Unpackspec fieldsR fieldsR,
              D.Default J.NullMaker fieldsR nullableFieldsR)
          => S.Select fieldsR
          -- ^ Right query
          -> S.SelectArr (fieldsR -> F.Field T.SqlBool) nullableFieldsR
          -- ^ Condition on which to join goes in, left join
          -- result comes out
leftJoinA = leftJoinAExplict D.def D.def

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

-- * Inferrable versions

leftJoinInferrable :: (D.Default U.Unpackspec fieldsL fieldsL,
                       D.Default U.Unpackspec fieldsR fieldsR,
                       D.Default J.NullMaker fieldsR nullableFieldsR,
                       Map.Map J.Nulled fieldsR ~ nullableFieldsR)
                   => S.Select fieldsL
                   -- ^ Left query
                   -> S.Select fieldsR
                   -- ^ Right query
                   -> ((fieldsL, fieldsR) -> F.Field T.SqlBool)
                   -- ^ Condition on which to join
                   -> S.Select (fieldsL, nullableFieldsR)
                   -- ^ Left join
leftJoinInferrable = leftJoin

rightJoinInferrable :: (D.Default U.Unpackspec fieldsL fieldsL,
                        D.Default U.Unpackspec fieldsR fieldsR,
                        D.Default J.NullMaker fieldsL nullableFieldsL,
                        Map.Map J.Nulled fieldsL ~ nullableFieldsL)
                    => S.Select fieldsL
                    -- ^ Left query
                    -> S.Select fieldsR
                    -- ^ Right query
                    -> ((fieldsL, fieldsR) -> F.Field T.SqlBool)
                    -- ^ Condition on which to join
                    -> S.Select (nullableFieldsL, fieldsR)
                    -- ^ Right join
rightJoinInferrable = rightJoin


fullJoinInferrable  :: (D.Default U.Unpackspec fieldsL fieldsL,
                        D.Default U.Unpackspec fieldsR fieldsR,
                        D.Default J.NullMaker fieldsL nullableFieldsL,
                        D.Default J.NullMaker fieldsR nullableFieldsR,
                        Map.Map J.Nulled fieldsL ~ nullableFieldsL,
                        Map.Map J.Nulled fieldsR ~ nullableFieldsR)
                    => S.Select fieldsL
                    -- ^ Left query
                    -> S.Select fieldsR
                    -- ^ Right query
                    -> ((fieldsL, fieldsR) -> F.Field T.SqlBool)
                    -- ^ Condition on which to join
                    -> S.Select (nullableFieldsL, nullableFieldsR)
                    -- ^ Full outer join
fullJoinInferrable = fullJoin

{-
fullJoinInferrableTF2  :: (D.Default U.Unpackspec fieldsL fieldsL,
                           D.Default U.Unpackspec fieldsR fieldsR,
                           D.Default J.NullMaker fieldsL nullableFieldsL,
                           D.Default J.NullMaker fieldsR nullableFieldsR,
                           (fieldsL :: *) ~
                           rec_ (hL :: TF2.Combinator (a20 'TF2.:-> 'TF2.BasicType a30)),
                           (fieldsR :: *) ~
                           rec_ (hR :: TF2.Combinator (a20 'TF2.:-> 'TF2.BasicType a30)),
                           (nullableFieldsL :: *) ~
                           rec_ (TF2.MapTFF J.Nulled hL),
                           (nullableFieldsR :: *) ~
                           rec_ (TF2.MapTFF J.Nulled hR))
                       => S.Select fieldsL
                       -- ^ Left query
                       -> S.Select fieldsR
                       -- ^ Right query
                       -> ((fieldsL, fieldsR) -> F.Field T.SqlBool)
                       -- ^ Condition on which to join
                       -> S.Select (nullableFieldsL, nullableFieldsR)
                       -- ^ Full outer join
fullJoinInferrableTF2 = fullJoin
-}

example :: (TF2.MapTFF J.Nulled TF2.O
           TF2.:**
           TF2.TableRecordField a T.SqlText TF2.NN TF2.Req)
           TF2.:~:
           Column (Nullable T.SqlText)
example = TF2.Refl
