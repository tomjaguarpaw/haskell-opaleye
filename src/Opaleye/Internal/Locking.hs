-- | Support for Postgres @SELECT@ locking clauses.
--
-- This functionality is new.  If you use it then please [open an
-- issue on
-- GitHub](https://github.com/tomjaguarpaw/haskell-opaleye/issues/new)
-- and let us know how it went, whether that is well or badly.
--
-- Not all Postgres locking clauses are supported.  If you need
-- another form of locking clause then please [open an issue on
-- GitHub](https://github.com/tomjaguarpaw/haskell-opaleye/issues/new).

module Opaleye.Internal.Locking where

import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.PrimQuery as PQ

-- | Adds a @FOR UPDATE@ clause to the 'Q.Select'.
--
-- Postgres has strong restrictions regarding the @SELECT@ clauses to
-- which a @FOR UPDATE@ can be added. Opaleye makes no attempt to
-- enforce those restrictions through its type system so it's very
-- easy to create queries that fail at run time using this operation.
forUpdate :: Q.Select a -> Q.Select a
forUpdate s = Q.productQueryArr $ do
  (a, query) <- Q.runSimpleSelect s
  pure (a, PQ.ForUpdate query)
