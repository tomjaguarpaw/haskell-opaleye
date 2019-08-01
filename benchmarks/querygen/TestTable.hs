{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-|
 Module : TableDef

Description : Basic table definitions to use when benchmarking


-}

module TestTable where

import Opaleye
import Data.Profunctor.Product.TH
import Data.Profunctor.Product

newtype TestTable' a = TestTable' {
  numberCol :: a
  }

type TestTableColR = TestTable'
  (Column PGInt4)

type TestTableColW = TestTableColR

$(makeAdaptorAndInstance  "pTestTable" ''TestTable')

tableTest :: Table TestTableColW TestTableColR
tableTest = Table "na" (pTestTable TestTable' {
                           numberCol = required "number_col"
                           })
