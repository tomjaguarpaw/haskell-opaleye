module Opaleye.With
  ( with,
    withMaterialized,
    withRecursive,
    withRecursiveDistinct,

    -- * Explicit versions
    withExplicit,
    withMaterializedExplicit,
    withRecursiveExplicit,
    withRecursiveDistinctExplicit,
  )
where

import Control.Category ((>>>))
import Control.Monad.Trans.State.Strict (State)
import Data.Profunctor.Product.Default (Default, def)
import Opaleye.Binary (unionAllExplicit, unionExplicit)
import Opaleye.Internal.Binary (Binaryspec (..))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import Opaleye.Internal.PackMap (PackMap (..))
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import Opaleye.Internal.QueryArr (Select, productQueryArr, runSimpleSelect)
import Opaleye.Internal.Rebind (rebindExplicitPrefixNoStar)
import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Internal.Tag as Tag
import Opaleye.Internal.Unpackspec (Unpackspec (..), runUnpackspec)

with :: Default Unpackspec a a => Select a -> (Select a -> Select b) -> Select b
with = withExplicit def

withMaterialized :: Default Unpackspec a a => Select a -> (Select a -> Select b) -> Select b
withMaterialized = withMaterializedExplicit def

-- | Denotionally, @withRecursive s f@ is the smallest set of rows @r@ such
-- that
--
-- @
-- r == s \`'unionAll'\` (r >>= f)
-- @
--
-- Operationally, @withRecursive s f@ takes each row in an initial set @s@ and
-- supplies it to @f@, resulting in a new generation of rows which are added
-- to the result set. Each row from this new generation is then fed back to
-- @f@, and this process is repeated until a generation comes along for which
-- @f@ returns an empty set for each row therein.
withRecursive :: Default Binaryspec a a => Select a -> (a -> Select a) -> Select a
withRecursive = withRecursiveExplicit def

-- | Denotationally, @withRecursiveDistinct s f@ is the smallest set of rows
-- @r@ such that
--
-- @
-- r == s \`'union'\` (r >>= f)
-- @
--
-- Operationally, @withRecursiveDistinct s f@ takes each /distinct/ row in an
-- initial set @s@ and supplies it to @f@, resulting in a new generation of
-- rows. Any rows returned by @f@ that already exist in the result set are not
-- considered part of this new generation by `withRecursiveDistinct` (in
-- contrast to `withRecursive`). This new generation is then added to the
-- result set, and each row therein is then fed back to @f@, and this process
-- is repeated until a generation comes along for which @f@ returns no rows
-- that don't already exist in the result set.
withRecursiveDistinct :: Default Binaryspec a a => Select a -> (a -> Select a) -> Select a
withRecursiveDistinct = withRecursiveDistinctExplicit def

withExplicit :: Unpackspec a a -> Select a -> (Select a -> Select b) -> Select b
withExplicit unpackspec rhsSelect bodySelect = productQueryArr $ do
  withG unpackspec PQ.NonRecursive Nothing (\_ -> rebind rhsSelect) bodySelect
  where
    rebind = (>>> rebindExplicitPrefixNoStar "rebind" unpackspec)

withMaterializedExplicit :: Unpackspec a a -> Select a -> (Select a -> Select b) -> Select b
withMaterializedExplicit unpackspec rhsSelect bodySelect = productQueryArr $ do
  withG unpackspec PQ.NonRecursive (Just PQ.Materialized) (\_ -> rebind rhsSelect) bodySelect
  where
    rebind = (>>> rebindExplicitPrefixNoStar "rebind" unpackspec)

withRecursiveExplicit :: Binaryspec a a -> Select a -> (a -> Select a) -> Select a
withRecursiveExplicit binaryspec base recursive = productQueryArr $ do
  let bodySelect selectCte = selectCte
  let rhsSelect selectCte = unionAllExplicit binaryspec base (selectCte >>= recursive)

  withG unpackspec PQ.Recursive Nothing rhsSelect bodySelect
  where
    unpackspec = binaryspecToUnpackspec binaryspec

withRecursiveDistinctExplicit :: Binaryspec a a -> Select a -> (a -> Select a) -> Select a
withRecursiveDistinctExplicit binaryspec base recursive = productQueryArr $ do
  let bodySelect selectCte = selectCte
  let rhsSelect selectCte = unionExplicit binaryspec base (selectCte >>= recursive)

  withG unpackspec PQ.Recursive Nothing rhsSelect bodySelect
  where
    unpackspec = binaryspecToUnpackspec binaryspec

withG ::
  Unpackspec a a ->
  PQ.Recursive ->
  Maybe PQ.Materialized ->
  (Select a -> Select a) ->
  (Select a -> Select b) ->
  State Tag.Tag (b, PQ.PrimQuery)
withG unpackspec recursive materialized rhsSelect bodySelect = do
  (selectCte, withCte) <- freshCte unpackspec

  let rhsSelect' = rhsSelect selectCte
  let bodySelect' = bodySelect selectCte

  (_, rhsQ) <- runSimpleSelect rhsSelect'
  bodyQ <- runSimpleSelect bodySelect'

  pure (withCte recursive materialized rhsQ bodyQ)

freshCte ::
  Unpackspec a a ->
  State
    Tag.Tag
    ( Select a,
      PQ.Recursive -> Maybe PQ.Materialized -> PQ.PrimQuery -> (b, PQ.PrimQuery) -> (b, PQ.PrimQuery)
    )
freshCte unpackspec = do
  cteName <- HPQ.Symbol "cte" <$> Tag.fresh

  -- TODO: Make a function that explicitly ignores its argument
  (cteColumns, cteBindings) <- do
    startTag <- Tag.fresh
    pure $
      PM.run $
        runUnpackspec unpackspec (PM.extractAttr "cte" startTag) (error "freshCte")

  let selectCte = productQueryArr $ do
        tag <- Tag.fresh
        let (renamedCte, renameCte) =
              PM.run $
                runUnpackspec unpackspec (PM.extractAttr "cte_renamed" tag) cteColumns

        pure (renamedCte, PQ.BaseTable (PQ.TableIdentifier Nothing (Sql.sqlSymbol cteName)) renameCte)

  pure
    ( selectCte,
      \recursive materialized withQ (withedCols, withedQ) ->
        (withedCols, PQ.With recursive materialized cteName (map fst cteBindings) withQ withedQ)
    )

binaryspecToUnpackspec :: Binaryspec a a -> Unpackspec a a
binaryspecToUnpackspec (Binaryspec (PackMap spec)) =
  Unpackspec $ PackMap $ \f a -> spec (\(pe, _) -> f pe) (a, a)
