{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Join where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PackMap             as PM
import qualified Opaleye.Internal.Tag                 as T
import qualified Opaleye.Internal.Unpackspec          as U
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
  def = NullMaker C.toNullable

instance D.Default NullMaker (Column (Nullable a)) (Column (Nullable a)) where
  def = NullMaker id

joinExplicit :: U.Unpackspec columnsA columnsA
             -> U.Unpackspec columnsB columnsB
             -> (columnsA -> returnedColumnsA)
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

          (newColumnsA, ljPEsA) =
            PM.run (U.runUnpackspec uA (extractLeftJoinFields 1 endTag) columnsA)
          (newColumnsB, ljPEsB) =
            PM.run (U.runUnpackspec uB (extractLeftJoinFields 2 endTag) columnsB)

          nullableColumnsA = returnColumnsA newColumnsA
          nullableColumnsB = returnColumnsB newColumnsB

          Column cond' = cond (columnsA, columnsB)
          primQueryR = PQ.Join joinType cond' ljPEsA ljPEsB primQueryA primQueryB

leftJoinAExplicit :: U.Unpackspec a a
                  -> NullMaker a nullableA
                  -> Q.Query a
                  -> Q.QueryArr (a -> Column T.PGBool) nullableA
leftJoinAExplicit uA nullmaker rq =
  Q.QueryArr $ \(p, primQueryL, t1) ->
    let (columnsR, primQueryR, t2) = Q.runSimpleQueryArr rq ((), t1)
        (newColumnsR, ljPEsR) = PM.run $ U.runUnpackspec uA (extractLeftJoinFields 2 t2) columnsR
        renamedNullable = toNullable nullmaker newColumnsR
        Column cond = p newColumnsR
    in ( renamedNullable
       , PQ.Join
           PQ.LeftJoin
           cond
           []
           --- ^ I am reasonably confident that we don't need any
           --- column names here.  Columns that can become NULL need
           --- to be written here so that we can wrap them.  If we
           --- don't constant columns can avoid becoming NULL.
           --- However, these are the left columns and cannot become
           --- NULL in a left join, so we are fine.
           ---
           --- Report about the "avoiding NULL" bug:
           ---
           ---     https://github.com/tomjaguarpaw/haskell-opaleye/issues/223
           ljPEsR
           primQueryL
           primQueryR
       , T.next t2)

extractLeftJoinFields :: Int
                      -> T.Tag
                      -> HPQ.PrimExpr
                      -> PM.PM [(HPQ.Symbol, HPQ.PrimExpr)] HPQ.PrimExpr
extractLeftJoinFields n = PM.extractAttr ("result" ++ show n ++ "_")

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
