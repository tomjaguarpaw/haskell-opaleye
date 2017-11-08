module Main where

import Opaleye.RunQuery
import Criterion.Main
import TestConnection

main :: IO ()
main = do
  conn <- getTestDbConnection
  pure ()
