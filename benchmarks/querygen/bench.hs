{-# LANGUAGE Arrows #-}

module Main where

import Data.Text (Text)
import Data.String
import Control.Arrow
import Criterion.Main
import TestTable
import Opaleye

-- function for generating the text with
-- showSqlBench :: Query a -> Maybe String
-- showSqlBench = fmap fromString . showSql
queryRanges :: (Int, Int) -- ^ Some range, assume the first is < the second
           -> Query (Column PGInt4)
queryRanges (min, max) = proc () -> do
  cols <- queryTable tableTest -< ()
  restrict -< numberCol cols .< toFields max .&& numberCol cols .>= toFields min
  returnA -< numberCol cols

countInRanges :: [(Int, Int)] -> Select (Column PGInt8)
countInRanges rs = aggregate count q
  where q = foldl1 unionAll (queryRanges <$> rs)

manyUnions :: Int -> Benchmark
manyUnions n = bench (show n <> " unions") $ nf showSql q
  where ranges = (\n -> (n, n + 10)) <$> iterate (+10) 0
        q = countInRanges (take n ranges)

onlyRows :: Benchmark
onlyRows = bench "Trivial querytable" $ nf showSql (queryTable tableTest)

manyBenchUnions :: [Benchmark]
manyBenchUnions = manyUnions <$> [5, 10, 20]

manyWideUnions :: [Benchmark]
manyWideUnions =
  let genUnion n = foldr1 unionAll (take n . repeat $ queryWideTable)
      genBench n = bench (show n <> " unions") $ nf showSql (genUnion n)
  in genBench <$> [5,10..30]

innerJoin :: [Benchmark]
innerJoin =
  let mkJoinQ wideTable accQ  = proc () -> do
        w <- wideTable >>> arr a -< ()
        w' <- accQ -< ()
        restrict -< w .== w'
        returnA -< w
      mkBench n =
        let bigQ = foldr mkJoinQ (queryWideTable >>> arr a) (take n $ repeat queryWideTable) in
          bench ("Inner join " <> show n <> " wide tables") $ nf showSql bigQ
  in
    mkBench <$> [5,10..30]
    

main :: IO ()
main = defaultMain [bgroup "Small Table" (onlyRows : manyBenchUnions)
                   , bgroup "Wide Table" manyWideUnions
                   , bgroup "Inner join" innerJoin]
