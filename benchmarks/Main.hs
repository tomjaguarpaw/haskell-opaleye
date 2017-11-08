{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


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

truncateTable :: PGS.Connection -> String -> IO ()
truncateTable conn tname = void $ PGS.execute_ conn (fromString $ "TRUNCATE TABLE " ++ tname)

type NarrowTableRowType  = ( Column PGInt4
                           , Column PGText
                           , Column PGText
                           , Column PGText
                           , Column PGText)
narrowTable :: O.Table NarrowTableRowType NarrowTableRowType
narrowTable = Table "narrow_table" $ PP.p5 ( O.required "id"
                                           , O.required "col1"
                                           , O.required "col2"
                                           , O.required "col3"
                                           , O.required "col4")

narrowTableQ :: Query NarrowTableRowType
narrowTableQ = queryTable narrowTable

main :: IO ()
main = do
  conn <- getTestDbConnection
  defaultMain
    [ bgroup "pepareQuery"
      [ bgroup "narrowTable"
        [ bench "findByPkCompleteRow" $ nf findByPk narrowTableQ
        , bench "findByPkSelect3" $ nf findByPkSelect3 narrowTableQ
        ]
      ]
    ]



instance NFData PGT.Query where
  rnf PGT.Query{PGT.fromQuery=q} = rnf q

instance NFData (FR.RowParser a) where
  rnf _ = ()

-- findByPk :: D.Default QueryRunner cols haskells
--          => Query cols
--          -> (Maybe PGS.Query, FR.RowParser haskells)
findByPk :: Query NarrowTableRowType
         -> (Maybe PGS.Query, FR.RowParser (Int, Text, Text, Text, Text))
findByPk q = OR.prepareQuery D.def pkQuery
  where
    pkQuery = proc () -> do
      r <- q -< ()
      let (id_, _, _, _, _) = r
      restrict -< id_ .== (pgInt4 1)
      returnA -< r

findByPkSelect3 :: Query NarrowTableRowType
         -> (Maybe PGS.Query, FR.RowParser (Int, Text, Text))
findByPkSelect3 q = OR.prepareQuery D.def pkQuery
  where
    pkQuery = proc () -> do
      r <- q -< ()
      let (id_, col1_, col2_, _, _) = r
      restrict -< id_ .== (pgInt4 1)
      returnA -< (id_, col1_, col2_)
