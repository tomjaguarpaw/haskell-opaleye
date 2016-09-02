{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Join where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Join as J
import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.Column (Column)
import qualified Opaleye.PGTypes as T

import qualified Data.Profunctor.Product.Default as D

-- | @leftJoin@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @leftJoin@.
--
-- Example specialization:
--
-- @
-- leftJoin :: Query (Column a, Column b)
--          -> Query (Column c, Column (Nullable d))
--          -> (((Column a, Column b), (Column c, Column (Nullable d))) -> Column 'Opaleye.PGTypes.PGBool')
--          -> Query ((Column a, Column b), (Column (Nullable c), Column (Nullable d)))
-- @
leftJoin  :: (D.Default U.Unpackspec columnsA columnsA,
              D.Default U.Unpackspec columnsB columnsB,
              D.Default J.NullMaker columnsB nullableColumnsB) =>
             Query columnsA -> Query columnsB
          -> ((columnsA, columnsB) -> Column T.PGBool)
          -> Query (columnsA, nullableColumnsB)
leftJoin = leftJoinExplicit D.def D.def D.def

rightJoin  :: (D.Default U.Unpackspec columnsA columnsA,
               D.Default U.Unpackspec columnsB columnsB,
               D.Default J.NullMaker columnsA nullableColumnsA) =>
              Query columnsA -> Query columnsB
           -> ((columnsA, columnsB) -> Column T.PGBool)
           -> Query (nullableColumnsA, columnsB)
rightJoin = rightJoinExplicit D.def D.def D.def


fullJoin  :: (D.Default U.Unpackspec columnsA columnsA,
              D.Default U.Unpackspec columnsB columnsB,
              D.Default J.NullMaker columnsA nullableColumnsA,
              D.Default J.NullMaker columnsB nullableColumnsB) =>
             Query columnsA -> Query columnsB
          -> ((columnsA, columnsB) -> Column T.PGBool)
          -> Query (nullableColumnsA, nullableColumnsB)
fullJoin = fullJoinExplicit D.def D.def D.def D.def

-- We don't actually need the Unpackspecs any more, but I'm going to
-- leave them here in case they're ever needed again.  I don't want to
-- have to break the API to add them back.
leftJoinExplicit :: U.Unpackspec columnsA columnsA
                 -> U.Unpackspec columnsB columnsB
                 -> J.NullMaker columnsB nullableColumnsB
                 -> Query columnsA -> Query columnsB
                 -> ((columnsA, columnsB) -> Column T.PGBool)
                 -> Query (columnsA, nullableColumnsB)
leftJoinExplicit _ _ nullmaker =
  J.joinExplicit id (J.toNullable nullmaker) PQ.LeftJoin

rightJoinExplicit :: U.Unpackspec columnsA columnsA
                  -> U.Unpackspec columnsB columnsB
                  -> J.NullMaker columnsA nullableColumnsA
                  -> Query columnsA -> Query columnsB
                  -> ((columnsA, columnsB) -> Column T.PGBool)
                  -> Query (nullableColumnsA, columnsB)
rightJoinExplicit _ _ nullmaker =
  J.joinExplicit (J.toNullable nullmaker) id PQ.RightJoin


fullJoinExplicit :: U.Unpackspec columnsA columnsA
                 -> U.Unpackspec columnsB columnsB
                 -> J.NullMaker columnsA nullableColumnsA
                 -> J.NullMaker columnsB nullableColumnsB
                 -> Query columnsA -> Query columnsB
                 -> ((columnsA, columnsB) -> Column T.PGBool)
                 -> Query (nullableColumnsA, nullableColumnsB)
fullJoinExplicit _ _ nullmakerA nullmakerB =
  J.joinExplicit (J.toNullable nullmakerA) (J.toNullable nullmakerB) PQ.FullJoin
