{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}


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
type NarrowTable  = ( Int
                    , Text
                    , Text
                    , Text
                    , Text)
narrowTable :: O.Table NarrowTablePG NarrowTablePG
narrowTable = Table "narrow_table" $ PP.p5 ( O.required "id"
                                           , O.required "col1"
                                           , O.required "col2"
                                           , O.required "col3"
                                           , O.required "col4")

narrowTableQ :: Query NarrowTablePG
narrowTableQ = queryTable narrowTable

main :: IO ()
main = do
  conn <- getTestDbConnection
  defaultMain
    [ bgroup "pepareQuery"
      [ bgroup "narrowTable"
        [ bench "findByPkSelect1" $ nf (filterAndSelect (Proxy :: Proxy Int) narrowTable pkFilter) (\r -> r ^. _1)
        , bench "findByPkSelect3" $ nf (filterAndSelect (Proxy :: Proxy (Int, Text, Text)) narrowTable pkFilter) (\r -> (r ^. _1, r ^. _2, r ^. _3))
        , bench "findByPkCompleteRow" $ nf (filterAndSelect (Proxy :: Proxy NarrowTable) narrowTable pkFilter) Prelude.id
        ]
      ]
    ]



instance NFData PGT.Query where
  rnf PGT.Query{PGT.fromQuery=q} = rnf q

instance NFData (FR.RowParser a) where
  rnf _ = ()

pkFilter :: Field1 r r (Column PGInt4) (Column PGInt4) => r -> Column PGBool
pkFilter r = (r ^. _1) .== (pgInt4 1)

filterAndSelect :: ( D.Default QueryRunner cols haskells
                   , (D.Default Unpackspec pgr pgr))
                => Proxy haskells
                -> Table pgw pgr
                -> (pgr -> Column PGBool)
                -> (pgr -> cols)
                -> (Maybe PGS.Query, FR.RowParser haskells)
filterAndSelect _ tbl filterFn selectorFn = OR.prepareQuery D.def pkQuery
  where
    pkQuery = proc () -> do
      r <- queryTable tbl -< ()
      restrict -< (filterFn r)
      returnA -< (selectorFn r)
