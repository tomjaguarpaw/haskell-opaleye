{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.RunSelect
  (-- * Running 'S.Select's
   runSelectI,
   runSelect,
   runSelectFold,
   -- * Cursor interface
   declareCursor,
   closeCursor,
   foldForward,
   -- * Creating new 'FromField's
   unsafeFromField,
   -- * Explicit versions
   runSelectExplicit,
   runSelectFoldExplicit,
   declareCursorExplicit,
   -- * Datatypes
   IRQ.Cursor,
   IRQ.FromFields,
   IRQ.FromField,
   IRQ.DefaultFromField(defaultFromField),
   -- * Helper functions
   IRQ.fromPGSFromField,
   IRQ.fromPGSFieldParser,
   -- * Deprecated
   runSelectTF,
   ) where

import qualified Database.PostgreSQL.Simple as PGS

import qualified Opaleye.Select as S
import qualified Opaleye.Internal.RunQueryExternal as RQ
import qualified Opaleye.TypeFamilies as TF
import           Opaleye.Internal.RunQuery (FromFields)
import qualified Opaleye.Internal.RunQuery as IRQ
import           Opaleye.Internal.Inferrable (Inferrable, runInferrable)

import qualified Data.Profunctor.Product.Default as D

-- | An alternative version of @runSelectI@ that is more general but
-- has worse type inference.  @runSelect@'s use of the @'D.Default'
-- 'FromFields'@
-- typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runSelect@.
runSelect :: D.Default FromFields fields haskells
          => PGS.Connection
          -- ^
          -> S.Select fields
          -- ^ ͘
          -> IO [haskells]
runSelect = RQ.runQuery

{-# DEPRECATED runSelectTF "Use 'runSelectI' instead." #-}
runSelectTF :: D.Default FromFields (rec TF.O) (rec TF.H)
            => PGS.Connection
            -- ^
            -> S.Select (rec TF.O)
            -- ^ ͘
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
  -- ^ ͘
  -> IO b
runSelectFold = RQ.runQueryFold

-- | Declare a temporary cursor. The cursor is given a unique name for the given
-- connection.
declareCursor
    :: D.Default FromFields fields haskells
    => PGS.Connection
    -- ^
    -> S.Select fields
    -- ^ ͘
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
    -- ^ ͘
    -> IO (Either a a)
foldForward = RQ.foldForward

-- | Use 'unsafeFromField' to make an instance to allow you to run
--   queries on your own datatypes.  For example:
--
-- @
-- newtype Foo = Foo Int
-- data SqlFoo
--
-- instance 'IRQ.DefaultFromField' SqlFoo Foo where
--    'IRQ.defaultFromField' = unsafeFromField Foo defaultFromField
-- @
--
-- It is \"unsafe\" because it does not check that the @sqlType@
-- correctly corresponds to the Haskell type.
unsafeFromField :: (b -> b')
                -> IRQ.FromField sqlType b
                -> IRQ.FromField sqlType' b'
unsafeFromField haskellF (IRQ.FromField fp) =
  fmap haskellF (IRQ.FromField fp)

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

-- | Example type specialization:
--
-- @
-- runSelectI :: 'S.Select' ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlInt4', 'Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlText') -> IO [(Int, String)]
-- @
--
-- Assuming the @makeAdaptorAndInstanceInferrable@ splice has been run for the product type @Foo@:
--
-- @
-- runSelectI :: 'S.Select' (Foo ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlInt4') ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlText') ('Opaleye.Field.Field' 'Opaleye.SqlTypes.SqlBool')
--            -> IO [Foo Int String Bool]
-- @
runSelectI :: (D.Default (Inferrable FromFields) fields haskells)
           => PGS.Connection
           -- ^
           -> S.Select fields
           -- ^ ͘
           -> IO [haskells]
runSelectI = RQ.runQueryExplicit (runInferrable D.def)
