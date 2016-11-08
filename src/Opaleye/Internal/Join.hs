{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Join where

import qualified Opaleye.Internal.Tag as T
import           Opaleye.Internal.Column (Column(Column), Nullable)
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.PGTypes as T
import qualified Opaleye.Column as C

import qualified Control.Applicative as A

import           Data.Profunctor (Profunctor, dimap)
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

newtype NullMaker a b = NullMaker (a -> b)

toNullable :: NullMaker a b -> a -> b
toNullable (NullMaker f) = f

instance D.Default NullMaker (Column a) (Column (Nullable a)) where
  -- TODO: This should probably be 'NullMaker C.toNullable'
  def = NullMaker C.unsafeCoerceColumn

instance D.Default NullMaker (Column (Nullable a)) (Column (Nullable a)) where
  -- TODO: This should probably be 'NullMaker id'
  def = NullMaker C.unsafeCoerceColumn

joinExplicit :: (columnsA -> returnedColumnsA)
             -> (columnsB -> returnedColumnsB)
             -> PQ.JoinType
             -> Q.Query columnsA -> Q.Query columnsB
             -> ((columnsA, columnsB) -> Column T.PGBool)
             -> Q.Query (returnedColumnsA, returnedColumnsB)
joinExplicit uA uB returnColumnsA returnColumnsB joinType
             qA qB cond = Q.simpleQueryArr q where
  q ((), startTag) = ((nullableColumnsA, nullableColumnsB), primQueryR, T.next endTag)
    where (columnsA, primQueryA, midTag) = Q.runSimpleQueryArr qA ((), startTag)
          (columnsB, primQueryB, endTag) = Q.runSimpleQueryArr qB ((), midTag)

          nullableColumnsA = returnColumnsA columnsA
          nullableColumnsB = returnColumnsB columnsB

          Column cond' = cond (columnsA, columnsB)
          primQueryR = PQ.Join joinType cond' primQueryA primQueryB


-- { Boilerplate instances

instance Functor (NullMaker a) where
  fmap f (NullMaker g) = NullMaker (fmap f g)

instance A.Applicative (NullMaker a) where
  pure = NullMaker . A.pure
  NullMaker f <*> NullMaker x = NullMaker (f A.<*> x)

instance Profunctor NullMaker where
  dimap f g (NullMaker h) = NullMaker (dimap f g h)

instance PP.ProductProfunctor NullMaker where
  empty  = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

--
