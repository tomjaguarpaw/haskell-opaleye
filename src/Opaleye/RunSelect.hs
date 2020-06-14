{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.RunSelect
  (module Opaleye.RunSelect,
   -- * Datatypes
   IRQ.Cursor,
   IRQ.FromFields,
   IRQ.FromField,
   IRQ.DefaultFromField,
   IRQ.defaultFromField,
   -- * Helper functions
   IRQ.fromPGSFromField,
   IRQ.fromPGSFieldParser) where

import qualified Data.Profunctor            as P
import qualified Database.PostgreSQL.Simple as PGS

import qualified Opaleye.Column as C
import qualified Opaleye.Select as S
import qualified Opaleye.RunQuery          as RQ
import qualified Opaleye.TypeFamilies as TF
import           Opaleye.Internal.RunQuery (FromFields)
import qualified Opaleye.Internal.RunQuery as IRQ
import           Opaleye.Internal.Inferrable (Inferrable, runInferrable)

import qualified Data.Profunctor.Product.Default as D

-- * Running 'S.Select's

-- | @runSelect@'s use of the @'D.Default' 'FromFields'@
-- typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runSelect@.
--
-- Example type specialization:
--
-- @
-- runSelect :: 'S.Select' ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlInt4', 'Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlText') -> IO [(Int, String)]
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- runSelect :: 'S.Select' (Foo ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlInt4') ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlText') ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlBool')
--           -> IO [Foo Int String Bool]
-- @
runSelect :: D.Default FromFields fields haskells
          => PGS.Connection
          -- ^
          -> S.Select fields
          -- ^
          -> IO [haskells]
runSelect = RQ.runQuery

-- | 'runSelectTF' has better type inference than 'runSelect' but only
-- works with "higher-kinded data" types.
runSelectTF :: D.Default FromFields (rec TF.O) (rec TF.H)
            => PGS.Connection
            -- ^
            -> S.Select (rec TF.O)
            -- ^
            -> IO [rec TF.H]
runSelectTF = RQ.runQuery

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

-- * Creating new 'FromField's

-- | Use 'unsafeFromField' to make an instance to allow you to run
--   queries on your own datatypes.  For example:
--
-- @
-- newtype Foo = Foo Int
--
-- instance DefaultFromField Foo Foo where
--    defaultFromField = unsafeFromField Foo defaultFromField
-- @
--
-- It is \"unsafe\" because it does not check that the @sqlType@
-- correctly corresponds to the Haskell type.
unsafeFromField :: (b -> b')
                -> IRQ.FromField sqlType b
                -> IRQ.FromField sqlType' b'
unsafeFromField haskellF qrc = IRQ.QueryRunnerColumn (P.lmap colF u)
                                                     (fmapFP haskellF fp)
  where IRQ.QueryRunnerColumn u fp = qrc
        fmapFP = fmap . fmap . fmap
        colF = C.unsafeCoerceColumn

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

-- | Version of 'runSelect' with better type inference
runSelectI :: (D.Default (Inferrable FromFields) fields haskells)
           => PGS.Connection
           -- ^
           -> S.Select fields
           -- ^
           -> IO [haskells]
runSelectI = RQ.runQueryExplicit (runInferrable D.def)
