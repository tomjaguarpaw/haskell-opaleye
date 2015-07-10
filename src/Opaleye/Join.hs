{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Join where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Join as J
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.QueryArr (Query)
import qualified Opaleye.Internal.QueryArr as Q
import           Opaleye.Internal.Column (Column(Column))
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

-- We don't actually need the Unpackspecs any more, but I'm going to
-- leave them here in case they're ever needed again.  I don't want to
-- have to break the API to add them back.
leftJoinExplicit :: U.Unpackspec columnsA columnsA
                 -> U.Unpackspec columnsB columnsB
                 -> J.NullMaker columnsB nullableColumnsB
                 -> Query columnsA -> Query columnsB
                 -> ((columnsA, columnsB) -> Column T.PGBool)
                 -> Query (columnsA, nullableColumnsB)
leftJoinExplicit _ _ nullmaker qA qB cond = Q.simpleQueryArr q where
  q ((), startTag) = ((columnsA, nullableColumnsB), primQueryR, T.next endTag)
    where (columnsA, primQueryA, midTag) = Q.runSimpleQueryArr qA ((), startTag)
          (columnsB, primQueryB, endTag) = Q.runSimpleQueryArr qB ((), midTag)

          nullableColumnsB = J.toNullable nullmaker columnsB

          Column cond' = cond (columnsA, columnsB)
          primQueryR = PQ.Join PQ.LeftJoin cond' primQueryA primQueryB
