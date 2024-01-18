{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.Internal.MaybeFields where

import           Control.Arrow (returnA, (<<<), (>>>))

import qualified Opaleye.Internal.Binary as B
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.ToFields as Constant
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import           Opaleye.Internal.Inferrable (Inferrable(Inferrable),
                                              runInferrable)
import qualified Opaleye.Internal.QueryArr as IQ
import qualified Opaleye.Internal.Rebind as Rebind
import qualified Opaleye.Internal.RunQuery as RQ
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Values as V
import           Opaleye.Select (Select, SelectArr)
import qualified Opaleye.Column
import qualified Opaleye.Field
import           Opaleye.Field (Field)
import           Opaleye.Internal.Operators ((.&&), (.||), (.==), restrict, not,
                                             ifExplict, IfPP, EqPP(EqPP))
import qualified Opaleye.Internal.Lateral
import qualified Opaleye.SqlTypes
import           Opaleye.SqlTypes (SqlBool, IsSqlType)

import           Control.Monad (replicateM_)

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as PP

import qualified Database.PostgreSQL.Simple.FromRow as PGSR

-- | The Opaleye analogue of 'Data.Maybe.Maybe'.  A value of type
-- @MaybeFields a@ either contains a value of type @a@, or it contains
-- nothing.
data MaybeFields fields =
  MaybeFields {
    mfPresent :: Opaleye.Column.Column Opaleye.SqlTypes.SqlBool
  , mfFields  :: fields
  }
  deriving Functor

instance Applicative MaybeFields where
  pure fields = MaybeFields { mfPresent = Opaleye.SqlTypes.sqlBool True
                            , mfFields  = fields
                            }
  MaybeFields t f <*> MaybeFields t' a =
    MaybeFields {
      mfPresent = t .&& t'
    , mfFields  = f a
    }

instance Monad MaybeFields where
  return = pure
  MaybeFields t a >>= f = case f a of
    MaybeFields t' b -> MaybeFields (t .&& t') b

-- | The Opaleye analogue of 'Data.Maybe.Nothing'.
nothingFields :: PP.Default V.Nullspec a a => MaybeFields a
nothingFields = nothingFieldsExplicit def
  where def :: PP.Default V.Nullspec a a => V.Nullspec a a
        def = PP.def

-- | The Opaleye analogue of @'Prelude.const' 'Data.Maybe.Nothing'@.
-- Can be useful to avoid type inference problems, because it doesn't
-- pick up a type class constraint.
nothingFieldsOfTypeOf :: a -> MaybeFields a
nothingFieldsOfTypeOf a = MaybeFields {
    mfPresent = Opaleye.SqlTypes.sqlBool False
  , mfFields  = a
  }

-- | The Opaleye analogue of 'Data.Maybe.Just'.  Equivalent to
-- 'Control.Applicative.pure'.
justFields :: a -> MaybeFields a
justFields = pure

-- | The Opaleye analogue of 'Data.Maybe.maybe'
maybeFields :: PP.Default IfPP b b => b -> (a -> b) -> MaybeFields a -> b
maybeFields = maybeFieldsExplicit PP.def

-- | Use a Haskell @\\case@ expression to pattern match on a
-- 'MaybeFields'.
--
-- @
-- example :: MaybeFields (Field SqlInt4) -> Field SqlInt4
-- example mf = matchMaybe mf $ \\case
--   Nothing -> 0
--   Just x  -> x * 100
-- @
matchMaybe :: PP.Default IfPP b b => MaybeFields a -> (Maybe a -> b) -> b
matchMaybe mf f = maybeFields (f Nothing) (f . Just) mf

-- | The Opaleye analogue of 'Data.Maybe.fromMaybe'
fromMaybeFields :: PP.Default IfPP b b => b -> MaybeFields b -> b
fromMaybeFields = fromMaybeFieldsExplicit PP.def

-- | The Opaleye analogue of 'Data.Maybe.maybeToList'. Unless you are
-- using arrow notation you'll probably find 'catMaybeFields' easier
-- to use.
maybeFieldsToSelect :: SelectArr (MaybeFields a) a
maybeFieldsToSelect = proc mf -> do
  restrict -< mfPresent mf
  returnA -< mfFields mf

-- | The Opaleye analogue of 'Data.Maybe.catMaybes'.  Most commonly
-- you will want to use this at type
--
-- @
-- catMaybeFields :: Select (MaybeFields a) -> Select a
-- @
catMaybeFields :: SelectArr i (MaybeFields a) -> SelectArr i a
catMaybeFields = (>>> maybeFieldsToSelect)

maybeFieldsExplicit :: IfPP b b' -> b -> (a -> b) -> MaybeFields a -> b'
maybeFieldsExplicit ifpp b f mf =
  ifExplict ifpp (mfPresent mf) (f (mfFields mf)) b

fromMaybeFieldsExplicit :: IfPP b b -> b -> MaybeFields b -> b
fromMaybeFieldsExplicit ifpp = flip (maybeFieldsExplicit ifpp) id

nothingFieldsExplicit :: V.Nullspec a b -> MaybeFields b
nothingFieldsExplicit = nothingFieldsOfTypeOf . V.nullFields

traverseMaybeFields :: SelectArr a b -> SelectArr (MaybeFields a) (MaybeFields b)
traverseMaybeFields query = proc mfInput -> do
  mfOutput <- optional (query <<< maybeFieldsToSelect) -< mfInput
  restrict -< mfPresent mfInput `implies` mfPresent mfOutput
  returnA -< MaybeFields (mfPresent mfInput) (mfFields mfOutput)

  where a `implies` b = Opaleye.Internal.Operators.not a .|| b

isJustAnd ::
  MaybeFields a ->
  (a -> Opaleye.Field.Field SqlBool) ->
  Opaleye.Field.Field SqlBool
isJustAnd ma cond = matchMaybe ma $ \case
  Nothing -> Opaleye.SqlTypes.sqlBool False
  Just a -> cond a

optional :: SelectArr i a -> SelectArr i (MaybeFields a)
optional = Opaleye.Internal.Lateral.laterally (optionalInternal (MaybeFields . isNotNull))
  where isNotNull = Opaleye.Internal.Operators.not . Opaleye.Field.isNull

optionalInternal :: (Opaleye.Field.FieldNullable SqlBool -> a -> r) -> Select a -> Select r
optionalInternal f query = IQ.leftJoinQueryArr' $ do
    -- This is basically a left join on TRUE, but Shane (@duairc)
    -- wrote it to ensure that we don't need an Unpackspec a a.
    let true = HPQ.ConstExpr (HPQ.BoolLit True)
    (r, right) <- IQ.runSimpleSelect $ proc () -> do
          a <- query -< ()
          true_ <- Rebind.rebind -< Opaleye.Field.toNullable (IC.Column true)
          returnA -< f true_ a
    pure $ \() -> (r, true, right)


-- | An example to demonstrate how the functionality of (lateral)
-- @LEFT JOIN@ can be recovered using 'optional'.
lateralLeftJoinOptional :: SelectArr i a
                        -> SelectArr i b
                        -> ((a, b) -> Opaleye.Field.Field Opaleye.SqlTypes.SqlBool)
                        -> SelectArr i (a, MaybeFields b)
lateralLeftJoinOptional fieldsL fieldsR cond = proc i -> do
  fieldsL' <- fieldsL -< i
  maybeFieldsR' <- optional (proc (fieldsL', i) -> do
                                fieldsR' <- fieldsR -< i
                                restrict -< cond (fieldsL', fieldsR')
                                returnA -< fieldsR'
                                ) -< (fieldsL', i)
  returnA -< (fieldsL', maybeFieldsR')

-- | An example to demonstrate how the functionality of
-- 'Opaleye.Join.optionalRestrict' can be recovered using 'optional'.
optionalRestrictOptional :: Select a
                         -> SelectArr (a -> Field SqlBool) (MaybeFields a)
optionalRestrictOptional q = optional $ proc cond -> do
  a <- q -< ()
  restrict -< cond a
  returnA -< a

-- | Convert @NULL@ to 'nothingFields' and non-@NULL@ to a 'justFields'
nullableToMaybeFields :: Opaleye.Field.FieldNullable a -> MaybeFields (Field a)
nullableToMaybeFields x = MaybeFields
  { mfPresent = Opaleye.Internal.Operators.not (Opaleye.Field.isNull x)
  , mfFields = unsafeFromNonNull x
  }
  where unsafeFromNonNull :: Opaleye.Field.FieldNullable a -> Field a
        unsafeFromNonNull = Opaleye.Field.unsafeCoerceField

-- | Convert 'nothingFields' to @NULL@ to a 'justFields' to non-@NULL@
maybeFieldsToNullable :: MaybeFields (Field a) -> Opaleye.Field.FieldNullable a
maybeFieldsToNullable x =
  IC.unsafeIfThenElse (mfPresent x)
                      (Opaleye.Field.toNullable (mfFields x))
                      Opaleye.Field.null

fromFieldsMaybeFields :: RQ.FromFields fields haskells
                      -> RQ.FromFields (MaybeFields fields) (Maybe haskells)
fromFieldsMaybeFields (RQ.FromFields u p c) = RQ.FromFields u' p' c'
  where u' = () <$ productProfunctorMaybeFields U.unpackspecField u

        p' = \mf -> do
          hIsPresent <- PGSR.field

          case hIsPresent of
            True  -> Just <$> p (mfFields mf)
            False -> Nothing <$ replicateM_ (c (mfFields mf))
                                            (PGSR.fieldWith (\_ _ -> pure ()))

        c' = \mf -> c (mfFields mf) + 1

-- | This is not safe in general because it relies on p not doing
-- anything observable with the @a@s if @mfPresent@ is false.  In
-- particular, it won't work for
-- 'Opaleye.Internal.Distinct.Distinctspec' because it does indeed
-- look at the @mfFields@ to check distinctness.
productProfunctorMaybeFields :: PP.ProductProfunctor p
                             => p (Field SqlBool) (Field SqlBool)
                             -> p a b
                             -> p (MaybeFields a) (MaybeFields b)
productProfunctorMaybeFields b p = MaybeFields PP.***$ P.lmap mfPresent b
                                               PP.**** P.lmap mfFields p

nullspecMaybeFields :: V.Nullspec a b
                    -> V.Nullspec (MaybeFields a) (MaybeFields b)
nullspecMaybeFields = productProfunctorMaybeFields V.nullspecField

unpackspecMaybeFields :: U.Unpackspec a b
                      -> U.Unpackspec (MaybeFields a) (MaybeFields b)
unpackspecMaybeFields = productProfunctorMaybeFields U.unpackspecField

valuesspecMaybeFields :: V.Valuesspec a b
                      -> V.Valuesspec (MaybeFields a) (MaybeFields b)
valuesspecMaybeFields = productProfunctorMaybeFields V.valuesspecField

toFieldsMaybeFields :: V.Nullspec a b
                    -> Constant.ToFields a b
                    -> Constant.ToFields (Maybe a) (MaybeFields b)
toFieldsMaybeFields n p = Constant.toToFields $ \case
  Nothing -> nothingFieldsExplicit n
  Just a  -> justFields (Constant.toFieldsExplicit p a)

ifPPMaybeFields :: IfPP a b -> IfPP (MaybeFields a) (MaybeFields b)
ifPPMaybeFields = productProfunctorMaybeFields PP.def

-- I'd rather not crack open EqPP to implement this but the
-- alternative is adding an operation eqPPOr :: EqPP a b -> EqPP a' b
-- -> EqPP (a, a') b, and possibly even more than that, so I can't be
-- bothered right now.
eqPPMaybeFields :: EqPP a b -> EqPP (MaybeFields a) (MaybeFields b)
eqPPMaybeFields (EqPP eqFields) = EqPP (\m1 m2 ->
    (mfPresent m1 .== mfPresent m2)
    .&& (mfPresent m1 `implies` eqFields (mfFields m1) (mfFields m2)))
  where a `implies` b = Opaleye.Internal.Operators.not a .|| b

-- | This is only safe if d is OK with having nulls passed through it
-- when they claim to be non-null.
unWithNulls :: PP.ProductProfunctor p
            => p (Field SqlBool) (Field SqlBool)
            -> WithNulls p a b
            -> p (MaybeFields a) (MaybeFields b)
unWithNulls b (WithNulls d) =
    MaybeFields PP.***$ P.lmap mfPresent b
                PP.**** d

newtype WithNulls p a b =
  WithNulls (p (MaybeFields a) b)

-- | This is only safe if @b@ is OK with having nulls passed through it
-- when they claim to be non-null.
mapMaybeFieldsWithNulls :: PP.ProductProfunctor p
                        => p (Field SqlBool) (Field SqlBool)
                        -> WithNulls p a b
                        -> WithNulls p (MaybeFields a) (MaybeFields b)
mapMaybeFieldsWithNulls b d =
  MaybeFields <$> P.lmap mfPresent (withNullsField b)
              <*> P.lmap mfFields d

-- | This is only safe if @col@ is OK with having nulls passed through it
-- when they claim to be non-null.
withNullsField :: (IsSqlType a, P.Profunctor p)
               => p (IC.Field_ n a) (IC.Field_ n a)
               -> WithNulls p (IC.Field_ n a) (IC.Field_ n a)
withNullsField col = result
  where result = WithNulls (P.lmap (\(MaybeFields b c) ->
                                      ifExplict PP.def b c nullC) col)
        nullC = V.nullFields V.nullspecField

binaryspecMaybeFields
  :: WithNulls B.Binaryspec a b
  -> B.Binaryspec (MaybeFields a) (MaybeFields b)
binaryspecMaybeFields = unWithNulls PP.def

instance P.Profunctor p => P.Profunctor (WithNulls p) where
  dimap f g (WithNulls d) = WithNulls (P.dimap (fmap f) g d)

instance P.Profunctor p => Functor (WithNulls p a) where
  fmap = P.rmap

instance PP.ProductProfunctor p => Applicative (WithNulls p a) where
  pure = WithNulls . PP.purePP
  WithNulls fd <*> WithNulls xd = WithNulls (fd PP.**** xd)

instance PP.ProductProfunctor p => PP.ProductProfunctor (WithNulls p) where
  purePP = pure
  (****) = (<*>)

instance PP.SumProfunctor p => PP.SumProfunctor (WithNulls p) where
  WithNulls ff +++! WithNulls xf =
    WithNulls (flip P.lmap (ff PP.+++! xf) $ \case
                  MaybeFields b (Left l)  -> Left  (MaybeFields b l)
                  MaybeFields b (Right r) -> Right (MaybeFields b r))

instance PP.Default RQ.FromFields fields haskells
  => PP.Default RQ.FromFields (MaybeFields fields) (Maybe haskells) where
  def = fromFieldsMaybeFields PP.def

instance PP.Default U.Unpackspec a b
  => PP.Default U.Unpackspec (MaybeFields a) (MaybeFields b) where
  def = unpackspecMaybeFields PP.def

instance PP.Default V.Valuesspec a b
  => PP.Default V.Valuesspec (MaybeFields a) (MaybeFields b) where
  def = valuesspecMaybeFields PP.def

instance (PP.Default Constant.ToFields a b, PP.Default V.Nullspec a b)
  => PP.Default Constant.ToFields (Maybe a) (MaybeFields b) where
  def = toFieldsMaybeFields PP.def PP.def

instance PP.Default IfPP a b
  => PP.Default IfPP (MaybeFields a) (MaybeFields b) where
  def = ifPPMaybeFields PP.def

instance PP.Default EqPP a b
  => PP.Default EqPP (MaybeFields a) (MaybeFields b) where
  def = eqPPMaybeFields PP.def

instance (P.Profunctor p, IsSqlType a, PP.Default p (IC.Field_ n a) (IC.Field_ n a))
  => PP.Default (WithNulls p) (IC.Field_ n a) (IC.Field_ n a) where
  def = withNullsField PP.def

instance PP.Default (WithNulls B.Binaryspec) a b
  => PP.Default B.Binaryspec (MaybeFields a) (MaybeFields b) where
  def = binaryspecMaybeFields PP.def

instance (PP.Default (Inferrable RQ.FromFields) fields haskells,
          Maybe haskells ~ maybe_haskells)
  => PP.Default (Inferrable RQ.FromFields) (MaybeFields fields) maybe_haskells where
  def = Inferrable (fromFieldsMaybeFields (runInferrable PP.def))

instance (PP.Default (Inferrable Constant.ToFields) a b, PP.Default V.Nullspec a b,
          MaybeFields b ~ maybeFields_b)
  => PP.Default (Inferrable Constant.ToFields) (Maybe a) maybeFields_b where
  def = Inferrable (toFieldsMaybeFields PP.def (runInferrable PP.def))
