{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}


module Main where

import Opaleye.Internal.RunQuery as OR
import Criterion.Main
import TestConnection
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Types as PGT
import qualified Database.PostgreSQL.Simple.FromRow as FR
import Data.Functor
import Data.String
import Control.Monad
import Opaleye as O
import qualified Data.Profunctor.Product as PP
import Control.Arrow
import qualified Data.Profunctor.Product.Default as D
import Data.Text
import Control.DeepSeq
import Control.Lens hiding ((.>), (.<))
import Prelude hiding (id)
import qualified Prelude(id)
import Data.Proxy
import Debug.Trace
import Text.InterpolatedString.Perl6 (qc)
import Data.List as DL
import Criterion.Types
import qualified System.Random as R

truncateTable :: PGS.Connection -> String -> IO ()
truncateTable conn tname = void $ PGS.execute_ conn ([qc|TRUNCATE TABLE {tname}|])

-- dropTable :: PGS.Connection -> String -> IO ()
-- dropTable conn tname = void $ PGS.execute_ conn ([qc|DROP TABLE IF EXISTS {tname}|])

createTable :: PGS.Connection -> String -> [(String, String)] -> IO ()
createTable conn tname cols = do
  void $ PGS.execute_ conn ([qc|DROP TABLE IF EXISTS {tname}|])
  void $ PGS.execute_ conn ([qc|CREATE TABLE {tname} ({colstring})|])
  void $ forM_ [1..1000] $ const $ PGS.execute conn ([qc|INSERT INTO {tname}(col1, col2, col3, col4) VALUES(?, ?, ?, ?)|]) ("text1" :: String, "text2" :: String, "text3" :: String, "text4" :: String)
  where
    colstring = DL.intercalate "," $ DL.map (\(cname, ctype) -> cname ++ " " ++ ctype) cols

type NarrowTablePG  = ( Column PGInt4
                      , Column PGText
                      , Column PGText
                      , Column PGText
                      , Column PGText)
type NarrowTablePGNullable  = ( Column (Nullable PGInt4)
                              , Column (Nullable PGText)
                              , Column (Nullable PGText)
                              , Column (Nullable PGText)
                              , Column (Nullable PGText))
type NarrowTable  = ( Int
                    , Text
                    , Text
                    , Text
                    , Text)
type NarrowTableMaybe  = ( Maybe Int
                         , Maybe Text
                         , Maybe Text
                         , Maybe Text
                         , Maybe Text)
narrowTable :: O.Table NarrowTablePG NarrowTablePG
narrowTable = Table "narrow_table" $ PP.p5 ( O.required "id"
                                           , O.required "col1"
                                           , O.required "col2"
                                           , O.required "col3"
                                           , O.required "col4")

narrowTableQ :: Query NarrowTablePG
narrowTableQ = queryTable narrowTable


narrowTable2 :: O.Table NarrowTablePG NarrowTablePG
narrowTable2 = Table "narrow_table2" $ PP.p5 ( O.required "id"
                                             , O.required "col1"
                                             , O.required "col2"
                                             , O.required "col3"
                                             , O.required "col4")

narrowTable2Q :: Query NarrowTablePG
narrowTable2Q = queryTable narrowTable


main :: IO ()
main = do
  traceIO "Acquiring database connection..."
  conn <- getTestDbConnection

  let narrowTableCols = [("id", "serial primary key"), ("col1", "text"), ("col2", "text"), ("col3", "text"), ("col4", "text")]
  traceIO "Dropping & re-creating test tables..."
  createTable conn "narrow_table" narrowTableCols
  createTable conn "narrow_table2" narrowTableCols

  -- nestedFilter <- forM [1..5] (const pkRangeFilter)

  let runQueryBenchmarks = [ ( "narrowTable/findBySelect1"
                             , nfIO $ runQueryS conn $ prepareQueryT (Proxy :: Proxy Int) narrowTable pkFilter (\r -> r ^. _1)
                             , nfIO $ queryWrapper_ (Proxy :: Proxy (PGT.Only Int)) conn ([qc|SELECT id FROM narrow_table WHERE id=1|]))
                           , ( "narrowTable/findByPkCompleteRow"
                             , nfIO $ runQueryS conn $ prepareQueryT (Proxy :: Proxy NarrowTable) narrowTable pkFilter Prelude.id
                             , nfIO $ queryWrapper_ (Proxy :: Proxy NarrowTable) conn ([qc|SELECT * FROM narrow_table WHERE id=1|]))
                           , ( "narrowQueryLefttJoin/Select1"
                             , nfIO $ runQueryS conn $ prepareQueryQ (Proxy :: Proxy Int) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1)) (\r -> r ^. _1._1)
                             , nfIO $ queryWrapper_  (Proxy :: Proxy (PGT.Only Int)) conn ([qc|SELECT narrow_table.id FROM narrow_table LEFT OUTER JOIN narrow_table2 ON narrow_table.id=narrow_table2.id WHERE narrow_table.id=1|]))
                           , ( "narrowQueryLefttJoin/CompleteRow"
                             , nfIO $ runQueryS conn $ prepareQueryQ (Proxy :: Proxy (NarrowTable, NarrowTableMaybe)) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1)) Prelude.id
                             , nfIO $ queryWrapper_ (Proxy :: Proxy (NarrowTable, NarrowTableMaybe)) conn ([qc|SELECT * FROM narrow_table LEFT OUTER JOIN narrow_table2 ON narrow_table.id=narrow_table2.id WHERE narrow_table.id=1|]))
                           ]

  defaultMain
   [
    bgroup "runQuery" $ (flip DL.map) runQueryBenchmarks $ \(bname, opaleyeQuery, handQuery) -> bgroup bname
                                                                                                [ bench "opaleye" opaleyeQuery
                                                                                                , bench "handwritten" opaleyeQuery
                                                                                                ]
    ]

  -- defaultMain
  --   [ bgroup "pepareQuery"
  --     [ bgroup "narrowTable"
  --       [ bench "findByPkSelect1" $ nf (prepareQueryT (Proxy :: Proxy Int) narrowTable pkFilter) (\r -> r ^. _1)
  --       -- , bench "findByPkSelect3" $ nf (prepareQueryT (Proxy :: Proxy (Int, Text, Text)) narrowTable pkFilter) (\r -> (r ^. _1, r ^. _2, r ^. _3))
  --       , bench "findByPkCompleteRow" $ nf (prepareQueryT (Proxy :: Proxy NarrowTable) narrowTable pkFilter) Prelude.id
  --       ]
  --     -- , bgroup "narrowQueryNested"
  --     --   [ bench "findByPkSelect1" $ nf (prepareQueryQ (Proxy :: Proxy Int) (nestQuery nestedFilter narrowTableQ) pkFilter) (\r -> r ^. _1)
  --     --   , bench "findByPkSelect3" $ nf (prepareQueryQ (Proxy :: Proxy (Int, Text, Text)) (nestQuery nestedFilter narrowTableQ) pkFilter) (\r -> (r ^. _1, r ^. _2, r ^. _3))
  --     --   , bench "findByPkCompleteRow" $ nf (prepareQueryQ (Proxy :: Proxy NarrowTable) (nestQuery nestedFilter narrowTableQ) pkFilter) Prelude.id
  --     --   ]
  --     , bgroup "narrowQueryLeftJoin"
  --       [ bench "findByPkSelect1" $ nf (prepareQueryQ (Proxy :: Proxy Int) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1))) (\r -> r ^. _1._1)
  --       -- , bench "findByPkSelect3" $ nf (prepareQueryQ (Proxy :: Proxy (Int, Text, Text)) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1))) (\r -> (r ^. _1._1, r ^. _1._2, r ^. _1._3))
  --       , bench "findByPkCompleteRow" $ nf (prepareQueryQ (Proxy :: Proxy (NarrowTable, NarrowTableMaybe)) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1))) Prelude.id
  --       ]
  --     ]
  --   ]

instance NFData PGT.Query where
  rnf PGT.Query{PGT.fromQuery=q} = rnf q

instance NFData (FR.RowParser a) where
  rnf _ = ()

instance (NFData a) => NFData (PGT.Only a) where
  rnf (PGT.Only a) = rnf a

instance ( NFData a1
         , NFData a2
         , NFData a3
         , NFData a4
         , NFData a5
         , NFData a6
         , NFData a7
         , NFData a8
         , NFData a9
         , NFData a10) => NFData (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  rnf (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = rnf a1 `seq`
                                                  rnf a2 `seq`
                                                  rnf a2 `seq`
                                                  rnf a3 `seq`
                                                  rnf a4 `seq`
                                                  rnf a5 `seq`
                                                  rnf a6 `seq`
                                                  rnf a7 `seq`
                                                  rnf a8 `seq`
                                                  rnf a9 `seq`
                                                  rnf a10

instance {-# OVERLAPS #-} FR.FromRow (NarrowTable, NarrowTableMaybe) where
  fromRow = (,) <$> FR.fromRow <*> FR.fromRow

nestQuery :: [cols -> Column PGBool]
          -> Query cols
          -> Query cols
nestQuery [] q = q
nestQuery (predicate:predicates) q = nestQuery predicates $ proc () -> do
  r <- q -< ()
  restrict -< (predicate r)
  returnA -< r

pkFilter :: Field1 r r (Column PGInt4) (Column PGInt4) => r -> Column PGBool
pkFilter r = (r ^. _1) .== (pgInt4 1)

pkRangeFilter :: Field1 r r (Column PGInt4) (Column PGInt4)
              => IO (r -> Column PGBool)
pkRangeFilter = do
  pk1 <- R.randomRIO (1, 1000)
  pk2 <- R.randomRIO (pk1, 1000)
  pure $ \r -> ((r ^. _1) .> (pgInt4 pk1)) .&& ((r ^. _1) .< (pgInt4 pk2))

queryBuilderT :: D.Default Unpackspec pgr pgr
             => Table pgw pgr
             -> (pgr -> Column PGBool)
             -> (pgr -> cols)
             -> Query cols
queryBuilderT tbl filterFn selectorFn = proc () -> do
  r <- queryTable tbl -< ()
  restrict -< (filterFn r)
  returnA -< (selectorFn r)


queryBuilderQ :: D.Default Unpackspec pgr pgr
             => Query pgr
             -> (pgr -> Column PGBool)
             -> (pgr -> cols)
             -> Query cols
queryBuilderQ q filterFn selectorFn = proc () -> do
  r <- q -< ()
  restrict -< (filterFn r)
  returnA -< (selectorFn r)


prepareQueryT :: ( D.Default QueryRunner cols haskells
                     , (D.Default Unpackspec pgr pgr))
                  => Proxy haskells
                  -> Table pgw pgr
                  -> (pgr -> Column PGBool)
                  -> (pgr -> cols)
                  -> (Maybe PGS.Query, FR.RowParser haskells)
prepareQueryT _ tbl filterFn selectorFn = OR.prepareQuery D.def $ queryBuilderT tbl filterFn selectorFn

runQueryS :: PGS.Connection
          -> (Maybe PGS.Query, FR.RowParser haskells)
          -> IO [haskells]
runQueryS conn (sql, parser) = maybe (return []) (PGS.queryWith_ parser conn) sql


prepareQueryQ :: ( D.Default QueryRunner cols haskells
                 , (D.Default Unpackspec pgr pgr))
              => Proxy haskells
              -> Query pgr
              -> (pgr -> Column PGBool)
              -> (pgr -> cols)
              -> (Maybe PGS.Query, FR.RowParser haskells)
prepareQueryQ _ q filterFn selectorFn = OR.prepareQuery D.def $ queryBuilderQ q filterFn selectorFn


narrowTableLeftJoin :: Query (NarrowTablePG, NarrowTablePGNullable)
narrowTableLeftJoin = O.leftJoin
  narrowTableQ
  narrowTable2Q
  (\(r1, r2) -> (r1 ^. _1) .== (r2 ^. _1))

queryWrapper_ :: (FR.FromRow a)
              => Proxy a
              -> PGS.Connection
              -> PGS.Query
              -> IO [a]
queryWrapper_ _ conn q = PGS.query_ conn q
