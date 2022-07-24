{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Values where

import           Opaleye.Internal.Column (Field_(Column))
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PGTypes
import qualified Opaleye.SqlTypes

import           Data.Functor.Identity (runIdentity)
import qualified Data.List.NonEmpty as NEL
import           Data.Profunctor (Profunctor, dimap, rmap, lmap)
import           Data.Profunctor.Product (ProductProfunctor)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)
import           Data.Void (Void, absurd)

import           Control.Applicative (Applicative, pure, (<*>))

-- FIXME: We don't currently handle the case of zero columns.  Need to
-- emit a dummy column and data.
valuesU :: U.Unpackspec columns columns'
        -> ValuesspecUnsafe columns columns'
        -> [columns]
        -> ((), T.Tag) -> (columns', PQ.PrimQuery)
valuesU unpack valuesspec rows ((), t) = (newColumns, primQ')
  where runRow row = valuesRow
           where (_, valuesRow) =
                   PM.run (U.runUnpackspec unpack extractValuesEntry row)

        (newColumns, valuesPEs_nulls) =
          PM.run (runValuesspec valuesspec (extractValuesField t))

        valuesPEs = map fst valuesPEs_nulls

        values :: [[HPQ.PrimExpr]]
        values = map runRow rows

        primQ' = case NEL.nonEmpty values of
          Nothing      -> PQ.Empty ()
          Just values' -> PQ.Values valuesPEs values'

-- We don't actually use the return value of this.  It might be better
-- to come up with another Applicative instance for specifically doing
-- what we need.
extractValuesEntry :: HPQ.PrimExpr -> PM.PM [HPQ.PrimExpr] HPQ.PrimExpr
extractValuesEntry pe = do
  PM.write pe
  return pe

extractValuesField :: T.Tag -> primExpr
                   -> PM.PM [(HPQ.Symbol, primExpr)] HPQ.PrimExpr
extractValuesField = PM.extractAttr "values"

newtype ValuesspecUnsafe columns columns' =
  Valuesspec (PM.PackMap () HPQ.PrimExpr () columns')

runValuesspec :: Applicative f => ValuesspecUnsafe columns columns'
              -> (() -> f HPQ.PrimExpr) -> f columns'
runValuesspec (Valuesspec v) f = PM.traversePM v f ()

instance Default ValuesspecUnsafe (Field_ n a) (Field_ n a) where
  def = Valuesspec (PM.iso id Column)

valuesUSafe :: Valuesspec columns columns'
            -> [columns]
            -> ((), T.Tag) -> (columns', PQ.PrimQuery)
valuesUSafe valuesspec@(ValuesspecSafe _ unpack) rows ((), t) =
  (newColumns, primQ')
  where runRow row =
          case PM.run (U.runUnpackspec unpack extractValuesEntry row) of
            (_, []) -> [zero]
            (_, xs) -> xs

        (newColumns, valuesPEs_nulls) =
          PM.run (runValuesspecSafe valuesspec (extractValuesField t))

        valuesPEs = map fst valuesPEs_nulls
        nulls = case map snd valuesPEs_nulls of
          []     -> [nullInt]
          nulls' -> nulls'

        yieldNoRows :: PQ.PrimQuery -> PQ.PrimQuery
        yieldNoRows = PQ.restrict (HPQ.ConstExpr (HPQ.BoolLit False))

        zero = HPQ.ConstExpr (HPQ.IntegerLit 0)
        nullInt = HPQ.CastExpr (Opaleye.Internal.PGTypes.showSqlType
                                  (Nothing :: Maybe Opaleye.SqlTypes.SqlInt4))
                               (HPQ.ConstExpr HPQ.NullLit)

        (values, wrap) = case NEL.nonEmpty rows of
          Nothing    -> (pure nulls, yieldNoRows)
          Just rows' -> (fmap runRow rows', id)

        primQ' = wrap (PQ.Values valuesPEs values)

data Valuesspec fields fields' =
  ValuesspecSafe (PM.PackMap HPQ.PrimExpr HPQ.PrimExpr () fields')
                 (U.Unpackspec fields fields')

{-# DEPRECATED ValuesspecSafe "Use Valuesspec instead.  Will be removed in version 0.10." #-}
type ValuesspecSafe = Valuesspec

runValuesspecSafe :: Applicative f
                  => Valuesspec columns columns'
                  -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                  -> f columns'
runValuesspecSafe (ValuesspecSafe v _) f = PM.traversePM v f ()

valuesspecField :: Opaleye.SqlTypes.IsSqlType a
                => Valuesspec (Field_ n a) (Field_ n a)
valuesspecField = def

instance Opaleye.Internal.PGTypes.IsSqlType a
  => Default Valuesspec (Field_ n a) (Field_ n a) where
  def = def_
    where def_ = ValuesspecSafe (PM.PackMap (\f () -> fmap Column (f null_)))
                                U.unpackspecField
          null_ = nullPE sqlType

          sqlType = columnProxy def_
          columnProxy :: f (Field_ n sqlType) -> Maybe sqlType
          columnProxy _ = Nothing

nullPE :: Opaleye.SqlTypes.IsSqlType a => proxy a -> HPQ.PrimExpr
nullPE sqlType = nullPEType (Opaleye.Internal.PGTypes.showSqlType sqlType)

nullPEType :: String -> HPQ.PrimExpr
nullPEType sqlType = HPQ.CastExpr sqlType (HPQ.ConstExpr HPQ.NullLit)

-- Implementing this in terms of Valuesspec for convenience
newtype Nullspec fields fields' = Nullspec (Valuesspec Void fields')

nullspecField :: Opaleye.SqlTypes.IsSqlType b
              => Nullspec a (Field_ n b)
nullspecField = Nullspec (lmap absurd valuesspecField)

nullspecList :: Nullspec a [b]
nullspecList = pure []

nullspecEitherLeft :: Nullspec a b
                   -> Nullspec a (Either b b')
nullspecEitherLeft = fmap Left

nullspecEitherRight :: Nullspec a b'
                    -> Nullspec a (Either b b')
nullspecEitherRight = fmap Right

instance Opaleye.SqlTypes.IsSqlType b
  => Default Nullspec a (Field_ n b) where
  def = nullspecField

-- | All fields @NULL@, even though technically the type may forbid
-- that!  Used to create such fields when we know we will never look
-- at them expecting to find something non-NULL.
nullFields :: Nullspec a fields -> fields
nullFields (Nullspec v) = runIdentity (runValuesspecSafe v pure)

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (ValuesspecUnsafe a) where
  fmap f (Valuesspec g) = Valuesspec (fmap f g)

instance Applicative (ValuesspecUnsafe a) where
  pure = Valuesspec . pure
  Valuesspec f <*> Valuesspec x = Valuesspec (f <*> x)

instance Profunctor ValuesspecUnsafe where
  dimap _ g (Valuesspec q) = Valuesspec (rmap g q)

instance ProductProfunctor ValuesspecUnsafe where
  purePP = pure
  (****) = (<*>)

instance Functor (Valuesspec a) where
  fmap f (ValuesspecSafe g h) = ValuesspecSafe (fmap f g) (fmap f h)

instance Applicative (Valuesspec a) where
  pure a = ValuesspecSafe (pure a) (pure a)
  ValuesspecSafe f f' <*> ValuesspecSafe x x' =
    ValuesspecSafe (f <*> x) (f' <*> x')

instance Profunctor Valuesspec where
  dimap f g (ValuesspecSafe q q') = ValuesspecSafe (rmap g q) (dimap f g q')

instance ProductProfunctor Valuesspec where
  purePP = pure
  (****) = (<*>)

instance Functor (Nullspec a) where
  fmap f (Nullspec g) = Nullspec (fmap f g)

instance Applicative (Nullspec a) where
  pure = Nullspec . pure
  Nullspec f <*> Nullspec x = Nullspec (f <*> x)

instance Profunctor Nullspec where
  dimap _ g (Nullspec q) = Nullspec (fmap g q)

instance ProductProfunctor Nullspec where
  purePP = pure
  (****) = (<*>)

-- }
