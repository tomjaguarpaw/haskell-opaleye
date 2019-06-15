-- | Left, right, and full outer joins.
--
-- The interface in this module is much nicer than the standard \"make
-- missing rows NULL\" interface that SQL provides.  If you really
-- want the standard interface then use "Opaleye.Join".

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opaleye.FunctionalJoin where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   ((<<<))

import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor.Product         as PP

import qualified Opaleye.Field                   as C
import qualified Opaleye.Field                   as F
import qualified Opaleye.Internal.Join           as IJ
import qualified Opaleye.Internal.Operators      as IO
import qualified Opaleye.Internal.Unpackspec     as IU
import qualified Opaleye.Join                    as J
import qualified Opaleye.Select                  as S
import qualified Opaleye.SqlTypes                as T
import qualified Opaleye.Operators               as O

joinF :: (fieldsL -> fieldsR -> fieldsResult)
      -- ^ Calculate result fields from input fields
      -> (fieldsL -> fieldsR -> F.Field T.SqlBool)
      -- ^ Condition on which to join
      -> S.Select fieldsL
      -- ^ Left query
      -> S.Select fieldsR
      -- ^ Right query
      -> S.Select fieldsResult
joinF f cond l r =
  fmap (uncurry f) (O.keepWhen (uncurry cond) <<< ((,) <$> l <*> r))

leftJoinF :: (D.Default IO.IfPP fieldsResult fieldsResult,
              D.Default IU.Unpackspec fieldsL fieldsL,
              D.Default IU.Unpackspec fieldsR fieldsR)
          => (fieldsL -> fieldsR -> fieldsResult)
          -- ^ Calculate result row from input rows for rows in the
          -- right query satisfying the join condition
          -> (fieldsL -> fieldsResult)
          -- ^ Calculate result row from input row when there are /no/
          -- rows in the right query satisfying the join condition
          -> (fieldsL -> fieldsR -> F.Field T.SqlBool)
          -- ^ Condition on which to join
          -> S.Select fieldsL
          -- ^ Left query
          -> S.Select fieldsR
          -- ^ Right query
          -> S.Select fieldsResult
leftJoinF f fL cond l r = fmap ret j
  where a1 = fmap (\x -> (x, T.sqlBool True))
        j  = J.leftJoinExplicit D.def
                                D.def
                                (PP.p2 (IJ.NullMaker id, nullmakerBool))
                                l
                                (a1 r)
                                (\(l', (r', _)) -> cond l' r')

        ret (lr, (rr, rc)) = O.ifThenElseMany (C.isNull rc) (fL lr) (f lr rr)

        nullmakerBool :: IJ.NullMaker (F.Field T.SqlBool)
                                      (F.FieldNullable T.SqlBool)
        nullmakerBool = D.def

rightJoinF :: (D.Default IO.IfPP fieldsResult fieldsResult,
               D.Default IU.Unpackspec fieldsL fieldsL,
               D.Default IU.Unpackspec fieldsR fieldsR)
           => (fieldsL -> fieldsR -> fieldsResult)
           -- ^ Calculate result row from input rows for rows in the
           -- left query satisfying the join condition
           -> (fieldsR -> fieldsResult)
           -- ^ Calculate result row from input row when there are /no/
           -- rows in the left query satisfying the join condition
           -> (fieldsL -> fieldsR -> F.Field T.SqlBool)
           -- ^ Condition on which to join
           -> S.Select fieldsL
           -- ^ Left query
           -> S.Select fieldsR
           -- ^ Right query
           -> S.Select fieldsResult
rightJoinF f fR cond l r = fmap ret j
  where a1 = fmap (\x -> (x, T.sqlBool True))
        j  = J.rightJoinExplicit D.def
                                 D.def
                                 (PP.p2 (IJ.NullMaker id, nullmakerBool))
                                 (a1 l)
                                 r
                                 (\((l', _), r') -> cond l' r')

        ret ((lr, lc), rr) = O.ifThenElseMany (C.isNull lc) (fR rr) (f lr rr)

        nullmakerBool :: IJ.NullMaker (F.Field T.SqlBool)
                                      (F.FieldNullable T.SqlBool)
        nullmakerBool = D.def

fullJoinF :: (D.Default IO.IfPP fieldsResult fieldsResult,
              D.Default IU.Unpackspec fieldsL fieldsL,
              D.Default IU.Unpackspec fieldsR fieldsR)
          => (fieldsL -> fieldsR -> fieldsResult)
           -- ^ Calculate result row from input rows for rows in the
           -- left and right query satisfying the join condition
          -> (fieldsL -> fieldsResult)
           -- ^ Calculate result row from left input row when there
           -- are /no/ rows in the right query satisfying the join
           -- condition
          -> (fieldsR -> fieldsResult)
           -- ^ Calculate result row from right input row when there
           -- are /no/ rows in the left query satisfying the join
           -- condition
          -> (fieldsL -> fieldsR -> F.Field T.SqlBool)
          -- ^ Condition on which to join
          -> S.Select fieldsL
          -- ^ Left query
          -> S.Select fieldsR
          -- ^ Right query
          -> S.Select fieldsResult
fullJoinF f fL fR cond l r = fmap ret j
  where a1 = fmap (\x -> (x, T.sqlBool True))
        j  = J.fullJoinExplicit D.def
                                D.def
                                (PP.p2 (IJ.NullMaker id, nullmakerBool))
                                (PP.p2 (IJ.NullMaker id, nullmakerBool))
                                (a1 l)
                                (a1 r)
                                (\((l', _), (r', _)) -> cond l' r')

        ret ((lr, lc), (rr, rc)) = O.ifThenElseMany (C.isNull lc)
                                     (fR rr)
                                     (O.ifThenElseMany (C.isNull rc)
                                        (fL lr)
                                        (f lr rr))

        nullmakerBool :: IJ.NullMaker (F.Field T.SqlBool)
                                      (F.FieldNullable T.SqlBool)
        nullmakerBool = D.def
