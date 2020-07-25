{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TutorialCoolRunner where

import TutorialCool

runThem :: IO ()
runThem = do
  run example1
  run example2
  run example3
  run example4
  run example5__1
  run example5__2
  run example5
  run example5_1
  run example5_2
  run example6
  run example7
  run example8
  run example9
  run example10
  run example11
  run example11_1
  run example11_2
  run example12

  run authorPaperCount
  run sameNamePapers
  run paperGaps
  run collaborators
  run sameNamePapersSimpler
  run paperGapsSimpler

  pure ()
