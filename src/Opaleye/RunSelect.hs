{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.RunSelect
  (module Opaleye.RunSelect,
   -- * Datatypes
   IRQ.Cursor,
   IRQ.FromFields,
   IRQ.FromField) where

import           Control.Applicative (pure, (<$>))
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Cursor  as PGSC
import qualified Database.PostgreSQL.Simple.FromRow as FR
import qualified Data.String as String

import           Opaleye.Column (Column)
import qualified Opaleye.Select as S
import qualified Opaleye.RunQuery          as RQ
import qualified Opaleye.Sql as S
import           Opaleye.Internal.RunQuery (FromFields)
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Internal.QueryArr as Q

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

-- * Running 'S.Select's

-- | @runSelect@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runSelect@.
--
-- Example type specialization:
--
-- @
-- runSelect :: 'S.Select' (Column 'Opaleye.SqlTypes.SqlInt4', Column 'Opaleye.SqlTypes.SqlText') -> IO [(Int, String)]
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- runSelect :: 'S.Select' (Foo (Column 'Opaleye.SqlTypes.SqlInt4') (Column 'Opaleye.SqlTypes.SqlText') (Column 'Opaleye.SqlTypes.SqlBool')
--           -> IO [Foo Int String Bool]
-- @
runSelect :: D.Default FromFields fields haskells
          => PGS.Connection
          -- ^
          -> S.Select fields
          -- ^
          -> IO [haskells]
runSelect = RQ.runQuery

-- | @runSelectFold@ streams the results of a query incrementally and consumes
-- the results with a left fold.
--
-- This fold is /not/ strict. The stream consumer is responsible for
-- forcing the evaluation of its result to avoid space leaks.
runSelectFold
  :: D.Default FromFields fields haskells
  => PGS.Connection
  -- ^
  -> S.Select fields
  -- ^
  -> b
  -- ^
  -> (b -> haskells -> IO b)
  -- ^
  -> IO b
runSelectFold = RQ.runQueryFold

-- * Cursor interface

-- | Declare a temporary cursor. The cursor is given a unique name for the given
-- connection.
--
-- Returns 'Nothing' when the query returns zero rows.
declareCursor
    :: D.Default FromFields fields haskells
    => PGS.Connection
    -- ^
    -> S.Select fields
    -- ^
    -> IO (IRQ.Cursor haskells)
declareCursor = RQ.declareCursor

-- | Close the given cursor.
closeCursor :: IRQ.Cursor fields -> IO ()
closeCursor = RQ.closeCursor

-- | Fold over a chunk of rows, calling the supplied fold-like function on each
-- row as it is received. In case the cursor is exhausted, a 'Left' value is
-- returned, otherwise a 'Right' value is returned.
foldForward
    :: IRQ.Cursor haskells
    -- ^
    -> Int
    -- ^
    -> (a -> haskells -> IO a)
    -- ^
    -> a
    -- ^
    -> IO (Either a a)
foldForward = RQ.foldForward

-- * Explicit versions

runSelectExplicit :: FromFields fields haskells
                  -> PGS.Connection
                  -> S.Select fields
                  -> IO [haskells]
runSelectExplicit = RQ.runQueryExplicit

runSelectFoldExplicit
  :: FromFields fields haskells
  -> PGS.Connection
  -> S.Select fields
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runSelectFoldExplicit = RQ.runQueryFoldExplicit

declareCursorExplicit
    :: FromFields fields haskells
    -> PGS.Connection
    -> S.Select fields
    -> IO (IRQ.Cursor haskells)
declareCursorExplicit = RQ.declareCursorExplicit
