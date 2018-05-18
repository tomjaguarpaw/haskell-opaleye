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
-- leftJoin :: Select (Column a, Column b)
--          -> Select (Column c, Column (Nullable d))
--          -> (((Column a, Column b), (Column c, Column (Nullable d))) -> Column 'Opaleye.SqlTypes.SqlBool')
--          -> Select ((Column a, Column b), (Column (Nullable c), Column (Nullable d)))
-- @

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Opaleye.Join where

import qualified Opaleye.Field               as F
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Join as J
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Map as Map
import qualified Opaleye.Select   as S
import qualified Opaleye.SqlTypes as T
import qualified Opaleye.PGTypes  as T
import           Opaleye.QueryArr (Query)
import           Opaleye.Column (Column)

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

leftJoinInferrable :: (D.Default U.Unpackspec columnsL columnsL,
                       D.Default U.Unpackspec columnsR columnsR,
                       D.Default J.NullMaker columnsR nullableColumnsR,
                       Map.Map J.Nulled columnsR ~ nullableColumnsR)
                   => Query columnsL
                   -- ^ Left query
                   -> Query columnsR
                   -- ^ Right query
                   -> ((columnsL, columnsR) -> Column T.PGBool)
                   -- ^ Condition on which to join
                   -> Query (columnsL, nullableColumnsR)
                   -- ^ Left join
leftJoinInferrable = leftJoin

rightJoinInferrable :: (D.Default U.Unpackspec columnsL columnsL,
                        D.Default U.Unpackspec columnsR columnsR,
                        D.Default J.NullMaker columnsL nullableColumnsL,
                        Map.Map J.Nulled columnsL ~ nullableColumnsL)
                    => Query columnsL
                    -- ^ Left query
                    -> Query columnsR
                    -- ^ Right query
                    -> ((columnsL, columnsR) -> Column T.PGBool)
                    -- ^ Condition on which to join
                    -> Query (nullableColumnsL, columnsR)
                    -- ^ Right join
rightJoinInferrable = rightJoin


fullJoinInferrable  :: (D.Default U.Unpackspec columnsL columnsL,
                        D.Default U.Unpackspec columnsR columnsR,
                        D.Default J.NullMaker columnsL nullableColumnsL,
                        D.Default J.NullMaker columnsR nullableColumnsR,
                        Map.Map J.Nulled columnsL ~ nullableColumnsL,
                        Map.Map J.Nulled columnsR ~ nullableColumnsR)
                    => Query columnsL
                    -- ^ Left query
                    -> Query columnsR
                    -- ^ Right query
                    -> ((columnsL, columnsR) -> Column T.PGBool)
                    -- ^ Condition on which to join
                    -> Query (nullableColumnsL, nullableColumnsR)
                    -- ^ Full outer join
fullJoinInferrable = fullJoin
