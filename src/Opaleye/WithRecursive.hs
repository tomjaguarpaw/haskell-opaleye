{-# LANGUAGE
    Arrows
  , FlexibleContexts
  #-}
module Opaleye.WithRecursive (withRecursive, withRecursiveExplicit) where

import Data.Profunctor.Product.Default (Default, def)
import Opaleye.Internal.QueryArr (runSimpleQueryArr, simpleQueryArr)
import Opaleye.Internal.Unpackspec (Unpackspec, runUnpackspec, collectPEs)
import Opaleye.QueryArr (Query, QueryArr)
import Opaleye.Internal.PackMap (PM)
import Opaleye.Internal.Tag (Tag)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PrimQuery           as PQ
import qualified Opaleye.Internal.Tag                 as Tag
import qualified Opaleye.Internal.PackMap as PM

withRecursive :: Default Unpackspec a a => Query a -> (QueryArr a a) -> Query a
withRecursive = withRecursiveExplicit def

withRecursiveExplicit :: Unpackspec a a -> Query a -> (QueryArr a a) -> Query a
withRecursiveExplicit unpack base recursive = simpleQueryArr q
  where
    q ((), startTag) = (newCols, primQueryEnd, Tag.next endTag)
      where
        (_       , primQBase, midTag) = runSimpleQueryArr base      (()      , startTag)
        -- Note that we're passing newCols here as input, since this
        -- one can recursively refer to the whole expression.
        (colsRec , primQRec , endTag) = runSimpleQueryArr recursive (newCols , midTag  )
        -- Generate an explicit column selection for the recursive
        -- query. We can't use *, since we later add a binding for the
        -- recursive table 'with_recursive_result'.
        (newCols, newAttrs) = PM.run $ runUnpackspec unpack (extractFields endTag) colsRec
        -- We need the right number of column names for the resulting
        -- total expression, we might as well take the ones we
        -- generated for the recursive query.
        newSelects = collectPEs unpack newCols
        primQueryEnd = PQ.WithRecursive
                         newSelects
                         newAttrs
                         (HPQ.Symbol "with_recursive_result" endTag)
                         primQBase
                         primQRec

extractFields :: Tag -> HPQ.PrimExpr
            -> PM [(HPQ.Symbol, HPQ.PrimExpr)] HPQ.PrimExpr
extractFields = PM.extractAttr ("rec" ++ "_")
