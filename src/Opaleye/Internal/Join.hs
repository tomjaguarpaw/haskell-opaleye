{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}

module Opaleye.Internal.Join where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PackMap             as PM
import qualified Opaleye.Internal.Tag                 as T
import qualified Opaleye.Internal.Unpackspec          as U
import           Opaleye.Internal.Column (Column(Column), Nullable)
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Operators as Op
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PGTypesExternal as T
import qualified Opaleye.SqlTypes as T
import qualified Opaleye.Column as C
import           Opaleye.Field   (Field)
import qualified Opaleye.Internal.Map as Map
import           Opaleye.Internal.MaybeFields (MaybeFields(MaybeFields),
                                               mfPresent, mfFields)
import qualified Opaleye.Select  as S
import qualified Opaleye.Internal.TypeFamilies as TF

import qualified Control.Applicative as A
import qualified Control.Arrow

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
             qA qB cond = Q.productQueryArr q where
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
          primQueryR = PQ.Join joinType cond'
                               (PQ.NonLateral, (PQ.Rebind True ljPEsA primQueryA))
                               (PQ.NonLateral, (PQ.Rebind True ljPEsB primQueryB))

leftJoinAExplicit :: U.Unpackspec a a
                  -> NullMaker a nullableA
                  -> Q.Query a
                  -> Q.QueryArr (a -> Column T.PGBool) nullableA
leftJoinAExplicit uA nullmaker rq =
  Q.QueryArr $ \(p, t1) ->
    let (columnsR, primQueryR, t2) = Q.runSimpleQueryArr rq ((), t1)
        (newColumnsR, ljPEsR) = PM.run $ U.runUnpackspec uA (extractLeftJoinFields 2 t2) columnsR
        renamedNullable = toNullable nullmaker newColumnsR
        Column cond = p newColumnsR
    in ( renamedNullable
       , \lat primQueryL -> PQ.Join
           PQ.LeftJoin
           cond
           (PQ.NonLateral, primQueryL)
           --- ^ I am reasonably confident that we don't need to rebind any
           --- column names here.  Columns that can become NULL need
           --- to be written here so that we can wrap them.  If we
           --- don't constant columns can avoid becoming NULL.
           --- However, these are the left columns and cannot become
           --- NULL in a left join, so we are fine.
           ---
           --- Report about the "avoiding NULL" bug:
           ---
           ---     https://github.com/tomjaguarpaw/haskell-opaleye/issues/223
           (lat, (PQ.Rebind True ljPEsR primQueryR))
       , T.next t2)

optionalRestrict :: D.Default U.Unpackspec a a
                 => S.Select a
                 -> S.SelectArr (a -> Field T.SqlBool) (MaybeFields a)
optionalRestrict = optionalRestrictExplicit D.def

optionalRestrictExplicit :: U.Unpackspec a a
                         -> S.Select a
                         -> S.SelectArr (a -> Field T.SqlBool) (MaybeFields a)
optionalRestrictExplicit uA q =
  dimap (. snd) (\(nonNullIfPresent, rest) ->
      let present = Op.not (C.isNull (C.unsafeCoerceColumn nonNullIfPresent))
      in MaybeFields { mfPresent = present
                     , mfFields  = rest
                     }) $
  leftJoinAExplicit (PP.p2 (U.unpackspecField, uA))
                    (Opaleye.Internal.Join.NullMaker id)
                    (fmap (\x -> (T.sqlBool True, x)) q)

-- | An example to demonstrate how the functionality of @LEFT JOIN@
-- can be recovered using 'optionalRestrict'.
leftJoinInTermsOfOptionalRestrict :: D.Default U.Unpackspec fieldsR fieldsR
                                  => S.Select fieldsL
                                  -> S.Select fieldsR
                                  -> ((fieldsL, fieldsR) -> Field T.SqlBool)
                                  -> S.Select (fieldsL, MaybeFields fieldsR)
leftJoinInTermsOfOptionalRestrict qL qR cond = proc () -> do
  fieldsL <- qL -< ()
  maybeFieldsR <- optionalRestrict qR -< curry cond fieldsL
  Control.Arrow.returnA -< (fieldsL, maybeFieldsR)

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
  purePP = pure
  (****) = (<*>)

--

{-# DEPRECATED Nulled "Will be removed in version 0.8" #-}
data Nulled

type instance TF.IMap Nulled TF.OT     = TF.NullsT
type instance TF.IMap Nulled TF.NullsT = TF.NullsT

-- It's quite unfortunate that we have to write these out by hand
-- until we probably do nullability as a distinction between
--
-- Column (Nullable a)
-- Column (NonNullable a)

type instance Map.Map Nulled (Column (Nullable a)) = Column (Nullable a)

type instance Map.Map Nulled (Column T.PGInt4) = Column (Nullable T.PGInt4)
type instance Map.Map Nulled (Column T.PGInt8) = Column (Nullable T.PGInt8)
type instance Map.Map Nulled (Column T.PGText) = Column (Nullable T.PGText)
type instance Map.Map Nulled (Column T.PGVarcharN) = Column (Nullable T.PGVarcharN)
type instance Map.Map Nulled (Column T.PGFloat8) = Column (Nullable T.PGFloat8)
type instance Map.Map Nulled (Column T.PGBool) = Column (Nullable T.PGBool)
type instance Map.Map Nulled (Column T.PGUuid) = Column (Nullable T.PGUuid)
type instance Map.Map Nulled (Column T.PGBytea) = Column (Nullable T.PGBytea)
type instance Map.Map Nulled (Column T.PGVarcharN) = Column (Nullable T.PGVarcharN)
type instance Map.Map Nulled (Column T.PGDate) = Column (Nullable T.PGDate)
type instance Map.Map Nulled (Column T.PGTimestamp) = Column (Nullable T.PGTimestamp)
type instance Map.Map Nulled (Column T.PGTimestamptz) = Column (Nullable T.PGTimestamptz)
type instance Map.Map Nulled (Column T.PGTime) = Column (Nullable T.PGTime)
type instance Map.Map Nulled (Column T.PGCitext) = Column (Nullable T.PGCitext)
type instance Map.Map Nulled (Column T.PGJson) = Column (Nullable T.PGJson)
type instance Map.Map Nulled (Column T.PGJsonb) = Column (Nullable T.PGJsonb)
