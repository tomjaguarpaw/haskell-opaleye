module Opaleye.Exists (exists) where

import           Opaleye.Field (Field)
import           Opaleye.Internal.Column (Column (Column))
import           Opaleye.Internal.QueryArr (runSimpleQueryArr, productQueryArr)
import           Opaleye.Internal.PackMap (run, extractAttr)
import           Opaleye.Internal.PrimQuery (PrimQuery' (Exists))
import           Opaleye.Internal.Tag (next)
import           Opaleye.Select (Select)
import           Opaleye.SqlTypes (SqlBool)

-- | True if any rows are returned by the given query, false otherwise.
--
-- This operation is equivalent to Postgres's @EXISTS@ operator.
exists :: Select a -> Select (Field SqlBool)
exists q = productQueryArr (f . runSimpleQueryArr q)
  where
    f (_, query, tag) = (Column result, Exists binding query, tag')
      where
        (result, [(binding, ())]) = run (extractAttr "exists" tag ())
        tag' = next tag
