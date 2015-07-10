{-# LANGUAGE FlexibleContexts #-}

module Opaleye.RunQuery (module Opaleye.RunQuery,
                         QueryRunner,
                         IRQ.QueryRunnerColumn,
                         IRQ.fieldQueryRunnerColumn) where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.FromRow as FR
import qualified Data.String as String

import           Opaleye.Column (Column)
import qualified Opaleye.Sql as S
import           Opaleye.QueryArr (Query)
import           Opaleye.Internal.RunQuery (QueryRunner(QueryRunner))
import qualified Opaleye.Internal.RunQuery as IRQ
import qualified Opaleye.Internal.QueryArr as Q

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

import           Control.Applicative ((*>))

-- | @runQuery@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runQuery@.
--
-- Example type specialization:
--
-- @
-- runQuery :: Query (Column 'Opaleye.PGTypes.PGInt4', Column 'Opaleye.PGTypes.PGText') -> IO [(Column Int, Column String)]
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- runQuery :: Query (Foo (Column 'Opaleye.PGTypes.PGInt4') (Column 'Opaleye.PGTypes.PGText') (Column 'Opaleye.PGTypes.PGBool')
--          -> IO [(Foo (Column Int) (Column String) (Column Bool)]
-- @
--
-- Opaleye types are converted to Haskell types based on instances of
-- the 'Opaleye.Internal.RunQuery.QueryRunnerColumnDefault' typeclass.
runQuery :: D.Default QueryRunner columns haskells
         => PGS.Connection
         -> Query columns
         -> IO [haskells]
runQuery = runQueryExplicit D.def

runQueryExplicit :: QueryRunner columns haskells
                 -> PGS.Connection
                 -> Query columns
                 -> IO [haskells]
runQueryExplicit (QueryRunner u rowParser nonZeroColumns) conn q =
  PGS.queryWith_ parser conn sql
  where sql :: PGS.Query
        sql = String.fromString (S.showSqlForPostgresExplicit u q)
        -- FIXME: We're doing work twice here
        (b, _, _) = Q.runSimpleQueryArrStart q ()
        parser = if nonZeroColumns b
                 then rowParser b
                 else (FR.fromRow :: FR.RowParser (PGS.Only Int)) *> rowParser b
                 -- If we are selecting zero columns then the SQL
                 -- generator will have to put a dummy 0 into the
                 -- SELECT statement, since we can't select zero
                 -- columns.  In that case we have to make sure we
                 -- read a single Int.

-- | Use 'queryRunnerColumn' to make an instance to allow you to run queries on
--   your own datatypes.  For example:
--
-- @
-- newtype Foo = Foo Int
-- instance Default QueryRunnerColumn Foo Foo where
--    def = queryRunnerColumn ('Opaleye.Column.unsafeCoerce' :: Column Foo -> Column PGInt4) Foo def
-- @
queryRunnerColumn :: (Column a' -> Column a) -> (b -> b')
                  -> IRQ.QueryRunnerColumn a b -> IRQ.QueryRunnerColumn a' b'
queryRunnerColumn colF haskellF qrc = IRQ.QueryRunnerColumn (P.lmap colF u)
                                                            (fmapFP haskellF fp)
  where IRQ.QueryRunnerColumn u fp = qrc
        fmapFP = fmap . fmap . fmap
