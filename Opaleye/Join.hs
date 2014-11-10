{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Join where

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import           Opaleye.Column (Column(Column), Nullable)
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Values as V

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product.Default as D

import qualified Database.HaskellDB.PrimQuery as HPQ

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
  q ((), startTag) = ((newColumnsA, nullableColumnsB), primQueryR, T.next endTag)
    where (columnsA, primQueryA, midTag) = Q.runSimpleQueryArr qA ((), startTag)
          (columnsB, primQueryB, endTag) = Q.runSimpleQueryArr qB ((), midTag)

          (newColumnsA, ljPEsA) =
            PM.run (U.runUnpackspec unpackA (extractLeftJoinFields 1 endTag) columnsA)
          (newColumnsB, ljPEsB) =
            PM.run (U.runUnpackspec unpackB (extractLeftJoinFields 2 endTag) columnsB)

          nullableColumnsB = toNullable nullmaker newColumnsB

          Column cond' = cond (columnsA, columnsB)
          primQueryR = PQ.Join PQ.LeftJoin (ljPEsA ++ ljPEsB) cond' primQueryA primQueryB

-- TODO: Move this to Internal
extractLeftJoinFields :: Int -> T.Tag -> HPQ.PrimExpr
            -> PM.PM [(String, HPQ.PrimExpr)] HPQ.PrimExpr
extractLeftJoinFields n = V.extractAttr (\i -> "result" ++ show n ++ "_" ++ i)

instance D.Default NullMaker (Column a) (Column (Nullable a)) where
  def = NullMaker C.unsafeCoerce

instance D.Default NullMaker (Column (Nullable a)) (Column (Nullable a)) where
  def = NullMaker C.unsafeCoerce

-- { Boilerplate instances

instance Profunctor NullMaker where
  dimap f g (NullMaker h) = NullMaker (dimap f g h)

instance ProductProfunctor NullMaker where
  empty = NullMaker empty
  NullMaker f ***! NullMaker f' = NullMaker (f ***! f')

--
