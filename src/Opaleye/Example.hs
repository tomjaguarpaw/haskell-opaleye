{-# LANGUAGE TemplateHaskell #-}

module Opaleye.Example where

import Opaleye
import Database.Postgres.Temp
import Database.PostgreSQL.Simple

exampleA = do
  i <- values [1, 2, 3]
  a <- aggregate sumInt4 $ do
    pure i
  pure a

example = (aggregate sumInt4 . pure) =<< values [1, 2, 3]


showIt :: IO ()
showIt = mapM_ putStrLn (showSql example)

runIt :: IO ()
runIt = do
  r <- with (\db -> do
                conn <- connectPostgreSQL (toConnectionString db)
                mapM_ print =<< runSelectI conn example)
  print r
