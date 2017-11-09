{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}


module Main where

import Opaleye.RunQuery as OR
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
import Control.Lens
import Prelude hiding (id)
import qualified Prelude(id)
import Data.Proxy

truncateTable :: PGS.Connection -> String -> IO ()
truncateTable conn tname = void $ PGS.execute_ conn (fromString $ "TRUNCATE TABLE " ++ tname)

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
  conn <- getTestDbConnection

  defaultMain
    [ bgroup "pepareQuery"
      [ bgroup "narrowTable"
        [ bench "findByPkSelect1" $ nf (prepareQueryT (Proxy :: Proxy Int) narrowTable pkFilter) (\r -> r ^. _1)
        , bench "findByPkSelect3" $ nf (prepareQueryT (Proxy :: Proxy (Int, Text, Text)) narrowTable pkFilter) (\r -> (r ^. _1, r ^. _2, r ^. _3))
        , bench "findByPkCompleteRow" $ nf (prepareQueryT (Proxy :: Proxy NarrowTable) narrowTable pkFilter) Prelude.id
        ]
      -- , bgroup "narrowQuery"
      --   [ bench "findByPkSelect1" $ nf (prepareQueryQ (Proxy :: Proxy Int) narrowTableQ pkFilter) (\r -> r ^. _1)
      --   , bench "findByPkSelect3" $ nf (prepareQueryQ (Proxy :: Proxy (Int, Text, Text)) narrowTableQ pkFilter) (\r -> (r ^. _1, r ^. _2, r ^. _3))
      --   , bench "findByPkCompleteRow" $ nf (prepareQueryQ (Proxy :: Proxy NarrowTable) narrowTableQ pkFilter) Prelude.id
      --   ]
      , bgroup "narrowQueryNested"
        [ bench "findByPkSelect1" $ nf (prepareQueryQ (Proxy :: Proxy Int) (nestQuery [pkFilter, pkFilter, pkFilter, pkFilter, pkFilter] narrowTableQ) pkFilter) (\r -> r ^. _1)
        , bench "findByPkSelect3" $ nf (prepareQueryQ (Proxy :: Proxy (Int, Text, Text)) (nestQuery [pkFilter, pkFilter, pkFilter, pkFilter, pkFilter] narrowTableQ) pkFilter) (\r -> (r ^. _1, r ^. _2, r ^. _3))
        , bench "findByPkCompleteRow" $ nf (prepareQueryQ (Proxy :: Proxy NarrowTable) (nestQuery [pkFilter, pkFilter, pkFilter, pkFilter, pkFilter] narrowTableQ) pkFilter) Prelude.id
        ]
      , bgroup "narrowQueryLeftJoin"
        [ bench "findByPkSelect1" $ nf (prepareQueryQ (Proxy :: Proxy Int) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1))) (\r -> r ^. _1._1)
        , bench "findByPkSelect3" $ nf (prepareQueryQ (Proxy :: Proxy (Int, Text, Text)) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1))) (\r -> (r ^. _1._1, r ^. _1._2, r ^. _1._3))
        , bench "findByPkCompleteRow" $ nf (prepareQueryQ (Proxy :: Proxy (NarrowTable, NarrowTableMaybe)) narrowTableLeftJoin (\r -> (r ^._1._1) .== (pgInt4 1))) Prelude.id
        ]
      ]
    ]



instance NFData PGT.Query where
  rnf PGT.Query{PGT.fromQuery=q} = rnf q

instance NFData (FR.RowParser a) where
  rnf _ = ()

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
  
