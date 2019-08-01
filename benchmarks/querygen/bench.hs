{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import Criterion.Main
import TestTable
import Opaleye


queryRanges :: (Int, Int) -- ^ Some range, assume the first is < the second
           -> Query (Column PGInt4)
queryRanges (min, max) = proc () -> do
  cols <- queryTable tableTest -< ()
  restrict -< numberCol cols .< toFields max .&& numberCol cols .>= toFields min
  returnA -< numberCol cols

countInRanges :: [(Int, Int)] -> Query (Column PGInt8)
countInRanges rs = aggregate count q
  where q = foldl1 unionAll (queryRanges <$> rs)

onlyRows :: Benchmark
onlyRows = bench "Trivial querytable" $ whnf showSql (queryTable tableTest) 

manyUnions :: Int -> Benchmark
manyUnions n = bench (show n <> " unions") $ whnf showSql q
  where ranges = (\n -> (n, n + 10)) <$> iterate (+10) 0
        q = countInRanges (take n ranges)

main :: IO ()
main = defaultMain [bgroup "textgen" benches]
  where benches = [onlyRows
                  , manyUnions 5 
                  , manyUnions 10 
                  , manyUnions 20]
