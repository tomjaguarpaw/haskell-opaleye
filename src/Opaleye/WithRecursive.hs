{-# LANGUAGE FlexibleContexts #-}

module Opaleye.WithRecursive
  ( withRecursive
  , withRecursiveExplicit
  )
where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Profunctor.Product.Default (Default, def)
import Opaleye.Internal.Binary (Binaryspec(..), runBinaryspec)
import Opaleye.Internal.QueryArr (Select, runSimpleQueryArr', productQueryArr')
import Opaleye.Internal.Unpackspec (Unpackspec(..), runUnpackspec)
import Opaleye.Internal.PackMap (PackMap(..))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PrimQuery           as PQ
import qualified Opaleye.Internal.Sql                 as Sql
import qualified Opaleye.Internal.Tag                 as Tag
import qualified Opaleye.Internal.PackMap as PM


withRecursive :: Default Binaryspec a a => Select a -> (a -> Select a) -> Select a
withRecursive = withRecursiveExplicit def


binaryspecToUnpackspec :: Binaryspec a a -> Unpackspec a a
binaryspecToUnpackspec (Binaryspec (PackMap spec)) =
  Unpackspec $ PackMap $ \f a -> spec (\(pe, _) -> f pe) (a, a)


withRecursiveExplicit :: Binaryspec a a -> Select a -> (a -> Select a) -> Select a
withRecursiveExplicit binaryspec base recursive = productQueryArr' $ \_ -> do
  tableName <- HPQ.Symbol "cte" <$> Tag.fresh

  let
    mkSelect :: PQ.Bindings HPQ.PrimExpr -> PQ.PrimQuery
    mkSelect = PQ.BaseTable (PQ.TableIdentifier Nothing (Sql.sqlSymbol tableName))

  (a, primQBase) <- runSimpleQueryArr' base ()

  startTag <- Tag.fresh

  let
    (columns, _) = PM.run $
      runUnpackspec unpackspec (PM.extractAttr "binary" startTag) a

  midTag <- Tag.fresh

  let
    (selected, bindings') = PM.run $
      runUnpackspec unpackspec (PM.extractAttr "rec" midTag) columns

  (b, primQRecursive) <- runSimpleQueryArr' (recursive selected) ()

  let
    select = mkSelect bindings'
    primQRecursive' = PQ.Product ((PQ.NonLateral, select) :| [(PQ.Lateral, primQRecursive)]) []

    (_, bindings) = PM.run $
      runBinaryspec binaryspec (PM.extractAttr "binary" startTag) (a, b)
    binaryQuery = PQ.Binary PQ.UnionAll
      ( PQ.Rebind False (map (fmap fst) bindings) primQBase
      , PQ.Rebind False (map (fmap snd) bindings) primQRecursive'
      )

  endTag <- Tag.fresh

  let
    (result, bindings'') = PM.run $
      runUnpackspec unpackspec (PM.extractAttr "with" endTag) columns

    select' = mkSelect bindings''

    withQuery = PQ.With PQ.Recursive tableName binaryQuery select'

  pure (result, withQuery)
  where
    unpackspec = binaryspecToUnpackspec binaryspec
