{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Opaleye.Example where

import Opaleye
import Opaleye.Join
import Database.Postgres.Temp
import Database.PostgreSQL.Simple

import Control.Arrow
import Data.Profunctor.Product

example :: Select (Field SqlInt4, Field SqlInt4, Field SqlInt8)
example = do
  i <- values [1 :: Field SqlInt4, 2, 3]
  a <- proc () -> do
      { x <- values [10 :: Field SqlInt4, 20, 30] -< ()
      ; y <- values [100 :: Field SqlInt4, 200, 300] -< ()
      ; z <- aggregate sumInt4 $ values [i * 10 :: Field SqlInt4, i * 20, i * 30] -< ()
      ; returnA -< (x, y, z)
      }

  pure a

example2 = do
  i <- values [1 :: Field SqlInt4]

  j <- Opaleye.Join.optional (values [i * 10])

  pure (i, j)


exampleB = (aggregate sumInt4 . pure) =<< values [1, 2, 3]


example3 = do
  i <- do
    i1 <- values [1 :: Field SqlInt4]
    values [i1 + 1]

  j <- do
    i1 <- values [i + 2 :: Field SqlInt4]
    values [i1 + 3]

  pure j

showIt :: IO ()
showIt = mapM_ putStrLn (showSql example)

runIt :: IO ()
runIt = do
  putStr "Launching..."
  r <- with (\db -> do
                putStr "Connecting..."
                conn <- connectPostgreSQL (toConnectionString db)
                putStrLn "Running..."
                mapM_ print =<< runSelectI conn example2)
  print r
