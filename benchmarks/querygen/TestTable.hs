{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-|
 Module : TableDef

Description : Basic table definitions to use when benchmarking


-}

module TestTable (TestTable'(..)
                 , TestTableWide(..)
                 , tableTest
                 , queryWideTable) where

import Opaleye
import Data.Profunctor.Product.TH

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

data TestTableWide a b c d e ff g h i j k l m n o p q r s t u v w x y z =
  TestTableWide {
  a :: a
  , b :: b
  , c :: c
  , d :: d
  , e :: e
  , ff :: ff
  , g :: g
  , h :: h
  , i :: i
  , j :: j
  , k :: k
  , l :: l
  , m :: m
  , n :: n
  , o :: o
  , p :: p
  , q :: q
  , r :: r
  , s :: s
  , t :: t
  , u :: u
  , v :: v
  , w :: w
  , x :: x
  , y :: y
  , z :: z
  }

$(makeAdaptorAndInstance  "pTestTableWide" ''TestTableWide)

type TTWideTextColR = TestTableWide
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)

type TTWideTextColW = TestTableWide
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)

wideTable :: Table TTWideTextColW TTWideTextColR
wideTable = Table "na" (pTestTableWide TestTableWide {
  a = required "a"
  , b = required "b"
  , c = required "c"
  , d = required "d"
  , e = required "e"
  , ff = required "ff"
  , g = required "g"
  , h = required "h"
  , i = required "i"
  , j = required "j"
  , k = required "k"
  , l = required "l"
  , m = required "m"
  , n = required "n"
  , o = required "o"
  , p = required "p"
  , q = required "q"
  , r = required "r"
  , s = required "s"
  , t = required "t"
  , u = required "u"
  , v = required "v"
  , w = required "w"
  , x = required "x"
  , y = required "y"
  , z = required "z"
  })


queryWideTable :: Select TTWideTextColR
queryWideTable = queryTable wideTable
