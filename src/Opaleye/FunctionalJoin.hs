{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Opaleye.FunctionalJoin where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   ((<<<))

import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor.Product         as PP

import qualified Opaleye.Column                  as C
import           Opaleye.Internal.Column         (Column, Nullable)
import qualified Opaleye.Internal.Join           as IJ
import qualified Opaleye.Internal.Operators      as IO
import qualified Opaleye.Internal.Unpackspec     as IU
import qualified Opaleye.Join                    as J
import qualified Opaleye.PGTypes                 as T
import qualified Opaleye.Operators               as O
import           Opaleye.QueryArr                (Query)

joinF :: (columnsL -> columnsR -> columnsResult)
      -> (columnsL -> columnsR -> Column T.PGBool)
      -> Query columnsL
      -> Query columnsR
      -> Query columnsResult
joinF f cond l r =
  fmap (uncurry f) (O.keepWhen (uncurry cond) <<< ((,) <$> l <*> r))

leftJoinF :: (D.Default IO.IfPP columnsResult columnsResult,
              D.Default IU.Unpackspec columnsL columnsL,
              D.Default IU.Unpackspec columnsR columnsR)
          => (columnsL -> columnsR -> columnsResult)
          -> (columnsL -> columnsResult)
          -> (columnsL -> columnsR -> Column T.PGBool)
          -> Query columnsL
          -> Query columnsR
          -> Query columnsResult
leftJoinF f fL cond l r = fmap ret j
  where a1 = fmap (\x -> (x, T.pgBool True))
        j  = J.leftJoinExplicit D.def
                                D.def
                                (PP.p2 ((IJ.NullMaker id), nullmakerBool))
                                l
                                (a1 r)
                                (\(l', (r', _)) -> cond l' r')

        ret (lr, (rr, rc)) = O.ifThenElseMany (C.isNull rc) (fL lr) (f lr rr)

        nullmakerBool :: IJ.NullMaker (Column T.PGBool)
                                      (Column (Nullable T.PGBool))
        nullmakerBool = D.def
