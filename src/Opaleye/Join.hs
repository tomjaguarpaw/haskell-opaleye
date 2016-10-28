-- | Left, right, and full outer joins.  If you want inner joins, just use 'restrict' instead.
--
-- The use of the 'D.Default' typeclass means that the compiler will
-- have trouble inferring types.  It is strongly recommended that you
-- provide full type signatures when using the join functions.
--
-- Example specialization:
--
-- @
-- leftJoin :: Query (Column a, Column b)
--          -> Query (Column c, Column (Nullable d))
--          -> (((Column a, Column b), (Column c, Column (Nullable d))) -> Column 'Opaleye.PGTypes.PGBool')
--          -> Query ((Column a, Column b), (Column (Nullable c), Column (Nullable d)))
-- @

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Join where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Join as J
import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.Column (Column)
import qualified Opaleye.PGTypes as T

import qualified Data.Profunctor.Product.Default as D

leftJoin  :: (D.Default U.Unpackspec columnsL columnsL,
              D.Default U.Unpackspec columnsR columnsR,
              D.Default J.NullMaker columnsR nullableColumnsR)
          => Query columnsL  -- ^ Left query
          -> Query columnsR  -- ^ Right query
          -> ((columnsL, columnsR) -> Column T.PGBool) -- ^ Condition on which to join
          -> Query (columnsL, nullableColumnsR) -- ^ Left join
leftJoin = leftJoinExplicit D.def D.def D.def

rightJoin  :: (D.Default U.Unpackspec columnsL columnsL,
               D.Default U.Unpackspec columnsR columnsR,
               D.Default J.NullMaker columnsL nullableColumnsL)
           => Query columnsL -- ^ Left query
           -> Query columnsR -- ^ Right query
           -> ((columnsL, columnsR) -> Column T.PGBool) -- ^ Condition on which to join
           -> Query (nullableColumnsL, columnsR) -- ^ Right join
rightJoin = rightJoinExplicit D.def D.def D.def


fullJoin  :: (D.Default U.Unpackspec columnsL columnsL,
              D.Default U.Unpackspec columnsR columnsR,
              D.Default J.NullMaker columnsL nullableColumnsL,
              D.Default J.NullMaker columnsR nullableColumnsR)
          => Query columnsL -- ^ Left query
          -> Query columnsR -- ^ Right query
          -> ((columnsL, columnsR) -> Column T.PGBool) -- ^ Condition on which to join
          -> Query (nullableColumnsL, nullableColumnsR) -- ^ Full outer join
fullJoin = fullJoinExplicit D.def D.def D.def D.def

-- We don't actually need the Unpackspecs any more, but I'm going to
-- leave them here in case they're ever needed again.  I don't want to
-- have to break the API to add them back.
leftJoinExplicit :: U.Unpackspec columnsL columnsL
                 -> U.Unpackspec columnsR columnsR
                 -> J.NullMaker columnsR nullableColumnsR
                 -> Query columnsL -> Query columnsR
                 -> ((columnsL, columnsR) -> Column T.PGBool)
                 -> Query (columnsL, nullableColumnsR)
leftJoinExplicit _ _ nullmaker =
  J.joinExplicit id (J.toNullable nullmaker) PQ.LeftJoin

rightJoinExplicit :: U.Unpackspec columnsL columnsL
                  -> U.Unpackspec columnsR columnsR
                  -> J.NullMaker columnsL nullableColumnsL
                  -> Query columnsL -> Query columnsR
                  -> ((columnsL, columnsR) -> Column T.PGBool)
                  -> Query (nullableColumnsL, columnsR)
rightJoinExplicit _ _ nullmaker =
  J.joinExplicit (J.toNullable nullmaker) id PQ.RightJoin


fullJoinExplicit :: U.Unpackspec columnsL columnsL
                 -> U.Unpackspec columnsR columnsR
                 -> J.NullMaker columnsL nullableColumnsL
                 -> J.NullMaker columnsR nullableColumnsR
                 -> Query columnsL -> Query columnsR
                 -> ((columnsL, columnsR) -> Column T.PGBool)
                 -> Query (nullableColumnsL, nullableColumnsR)
fullJoinExplicit _ _ nullmakerA nullmakerB =
  J.joinExplicit (J.toNullable nullmakerA) (J.toNullable nullmakerB) PQ.FullJoin
