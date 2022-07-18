module Opaleye.Exists (exists) where

import           Opaleye.Field (Field)
import           Opaleye.Internal.Column (Field_(Column))
import           Opaleye.Internal.QueryArr (productQueryArr, runSimpleQueryArr')
import           Opaleye.Internal.PackMap (run, extractAttr)
import           Opaleye.Internal.PrimQuery (PrimQuery' (Exists))
import           Opaleye.Internal.Tag (fresh)
import           Opaleye.Select (Select)
import           Opaleye.SqlTypes (SqlBool)

-- | True if any rows are returned by the given query, false otherwise.
--
-- This operation is equivalent to Postgres's @EXISTS@ operator.
exists :: Select a -> Select (Field SqlBool)
exists q = productQueryArr $ do
  (_, query) <- runSimpleQueryArr' q ()
  tag <- fresh
  let (result, [(binding, ())]) = run (extractAttr "exists" tag ())
  pure (Column result, Exists binding query)
