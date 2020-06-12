{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Opaleye.Internal.MaybeFields where

import           Control.Applicative hiding (optional)
import           Control.Arrow (returnA, (<<<))

import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.QueryArr as IQ
import qualified Opaleye.Internal.RunQuery as RQ
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Unpackspec as U
import           Opaleye.Select (Select, SelectArr)
import qualified Opaleye.Column
import qualified Opaleye.Field
import           Opaleye.Field (Field)
import           Opaleye.Operators ((.&&), (.||), restrict, not)
import           Opaleye.Internal.Operators (ifExplict, IfPP)
import qualified Opaleye.Internal.Lateral
import qualified Opaleye.SqlTypes
import           Opaleye.SqlTypes (SqlBool)

import           Control.Monad (replicateM_)

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as PP

import qualified Database.PostgreSQL.Simple.FromRow as PGSR

-- | The Opaleye analogue of 'Data.Maybe.Maybe'
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

-- | The Opaleye analogue of 'Data.Maybe.maybe'
maybeFields :: PP.Default IfPP b b => b -> (a -> b) -> MaybeFields a -> b
maybeFields = maybeFieldsExplicit PP.def

-- | The Opaleye analogue of 'Data.Maybe.fromMaybe'
fromMaybeFields :: PP.Default IfPP b b => b -> MaybeFields b -> b
fromMaybeFields = fromMaybeFieldsExplicit PP.def

-- | The Opaleye analogue of 'Data.Maybe.catMaybes'
catMaybeFields :: SelectArr (MaybeFields a) a
catMaybeFields = proc mf -> do
  restrict -< mfPresent mf
  returnA -< mfFields mf

maybeFieldsExplicit :: IfPP b b' -> b -> (a -> b) -> MaybeFields a -> b'
maybeFieldsExplicit ifpp b f mf =
  ifExplict ifpp (mfPresent mf) (f (mfFields mf)) b

fromMaybeFieldsExplicit :: IfPP b b -> b -> MaybeFields b -> b
fromMaybeFieldsExplicit ifpp = flip (maybeFieldsExplicit ifpp) id

traverseMaybeFields :: SelectArr a b -> SelectArr (MaybeFields a) (MaybeFields b)
traverseMaybeFields query = proc mfInput -> do
  mfOutput <- optional (query <<< catMaybeFields) -< mfInput
  restrict -< mfPresent mfInput `implies` mfPresent mfOutput
  returnA -< MaybeFields (mfPresent mfInput) (mfFields mfOutput)

  where a `implies` b = Opaleye.Operators.not a .|| b

-- | Convenient access to lateral left/right join
-- functionality. Performs a @LATERAL LEFT JOIN@ under the hood and
-- has behaviour equivalent to the following Haskell function:
--
-- @
-- optional :: [a] -> [Maybe a]
-- optional q = case q of
--     [] -> [Nothing]
--     xs -> map Just xs
-- @
--
-- That is, if @q :: 'SelectArr' i a@ returns no rows, @'optional' q
-- :: 'SelectArr' i ('MaybeFields' a)@ returns exactly one \"Nothing\"
-- row.  Otherwise, @'optional' q@ returns exactly the rows of @q@
-- wrapped in \"Just\".
--
-- @
-- > let l1 = ["one", "two", "three"] :: [Field SqlText]
-- > 'Opaleye.RunSelect.runSelect' conn ('optional' ('Opaleye.Values.valuesSafe' l1)) :: IO [Maybe String]
-- [Just "one", Just "two", Just "three"]
--
-- > let l2 = [] :: [Field SqlText]
-- > 'Opaleye.RunSelect.runSelect' conn ('optional' ('Opaleye.Values.valuesSafe' l2)) :: IO [Maybe String]
-- [Nothing]
-- @
--
-- @optional@ is a generalisation of 'Opaleye.Join.optionalRestrict'.
-- See the implementation of 'lateralLeftJoinOptional' for a
-- demonstration of how @LEFT JOIN@ can be written using 'optional'.
optional :: SelectArr i a -> SelectArr i (MaybeFields a)
optional = Opaleye.Internal.Lateral.laterally (IQ.QueryArr . go)
  where
    go query (i, left, tag) = (MaybeFields present a, join, Tag.next tag')
      where
        (MaybeFields t a, right, tag') =
          IQ.runSimpleQueryArr (pure <$> query) (i, tag)

        present = Opaleye.Operators.not (Opaleye.Field.isNull (IC.unsafeCoerceColumn t'))

        (t', bindings) =
          PM.run (U.runUnpackspec U.unpackspecColumn (PM.extractAttr "maybe" tag') t)
        join = PQ.Join PQ.LeftJoin true [] bindings left right
    true = HPQ.ConstExpr (HPQ.BoolLit True)

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
optionalRestrictOption :: Select a
                       -> SelectArr (a -> Field SqlBool) (MaybeFields a)
optionalRestrictOption q = optional $ proc cond -> do
  a <- q -< ()
  restrict -< cond a
  returnA -< a

fromFieldsMaybeFields :: RQ.FromFields fields haskells
                      -> RQ.FromFields (MaybeFields fields) (Maybe haskells)
fromFieldsMaybeFields (RQ.QueryRunner u p c) = RQ.QueryRunner u' p' c'
  where u' = () <$ P.lmap mfPresent U.unpackspecColumn
                <* P.lmap mfFields u

        p' = \mf -> do
          hIsPresent <- PGSR.field

          case hIsPresent of
            True  -> Just <$> p (mfFields mf)
            False -> Nothing <$ replicateM_ (c (mfFields mf))
                                            (PGSR.fieldWith (\_ _ -> pure ()))

        c' = \mf -> c (mfFields mf) + 1

unpackspecMaybeFields :: U.Unpackspec a b
                      -> U.Unpackspec (MaybeFields a) (MaybeFields b)
unpackspecMaybeFields u = MaybeFields <$> P.lmap mfPresent U.unpackspecColumn
                                      <*> P.lmap mfFields u

instance PP.Default RQ.QueryRunner fields haskells
  => PP.Default RQ.QueryRunner (MaybeFields fields) (Maybe haskells) where
  def = fromFieldsMaybeFields PP.def

instance PP.Default U.Unpackspec a b
  => PP.Default U.Unpackspec (MaybeFields a) (MaybeFields b) where
  def = unpackspecMaybeFields PP.def
