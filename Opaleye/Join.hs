{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Join where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.PrimQuery as PQ
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import           Opaleye.Column (Column(Column), Nullable)
import qualified Opaleye.Column as C

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product.Default as D

import qualified Database.HaskellDB.PrimQuery as HPQ

import qualified Control.Monad.Trans.State as S

data NullMaker a b = NullMaker (a -> b)

toNullable :: NullMaker a b -> a -> b
toNullable (NullMaker f) = f

leftJoin  :: (D.Default U.Unpackspec columnsA columnsA,
              D.Default U.Unpackspec columnsB columnsB,
              D.Default NullMaker columnsB nullableColumnsB) =>
             Query columnsA -> Query columnsB
          -> ((columnsA, columnsB) -> Column Bool)
          -> Query (columnsA, nullableColumnsB)
leftJoin = leftJoinExplicit D.def D.def D.def

leftJoinExplicit :: U.Unpackspec columnsA columnsA
                 -> U.Unpackspec columnsB columnsB
                 -> NullMaker columnsB nullableColumnsB
                 -> Query columnsA -> Query columnsB
                 -> ((columnsA, columnsB) -> Column Bool)
                 -> Query (columnsA, nullableColumnsB)
leftJoinExplicit unpackA unpackB nullmaker qA qB cond = Q.simpleQueryArr q where
  q ((), startTag) = ((newColumnsA, nullableColumnsB), primQueryR, endTag)
    where (columnsA, primQueryA, midTag) = Q.runSimpleQueryArr qA ((), startTag)
          (columnsB, primQueryB, endTag) = Q.runSimpleQueryArr qB ((), midTag)

          f :: Int -> HPQ.PrimExpr
            -> S.State ([(String, HPQ.PrimExpr)], Int) HPQ.PrimExpr
          f n pe = do
            (ljPEs, i) <- S.get
            let s = "result" ++ show n ++ "_" ++ show i
            S.put (ljPEs ++ [(s, pe)], i + 1)
            return (HPQ.AttrExpr s)

          (newColumnsA, (ljPEsA, _)) =
            S.runState (U.runUnpackspec unpackA (f 1) columnsA) ([], 0)
          (newColumnsB, (ljPEsB, _)) =
            S.runState (U.runUnpackspec unpackB (f 2) columnsB) ([], 0)

          nullableColumnsB = toNullable nullmaker newColumnsB

          Column cond' = cond (columnsA, columnsB)
          primQueryR = PQ.LeftJoin (ljPEsA ++ ljPEsB) cond' primQueryA primQueryB


instance D.Default NullMaker (Column a) (Column (Nullable a)) where
  def = NullMaker C.unsafeCoerce

-- { Boilerplate instances

instance Profunctor NullMaker where
  dimap f g (NullMaker h) = NullMaker (dimap f g h)

instance ProductProfunctor NullMaker where
  empty = NullMaker empty
  NullMaker f ***! NullMaker f' = NullMaker (f ***! f')

--
