{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Opaleye.Table as T
import           Opaleye.Table (TableColumn)
import           Opaleye.Column (Column)
import           Opaleye.Operators ((.==))
import qualified Opaleye.Operators as O
import           Opaleye.QueryArr (Query)
import qualified Opaleye.RunQuery as RQ

import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Profunctor.Product.Default as D
import qualified Data.List as L
import qualified System.Exit as Exit

import qualified Control.Applicative as A
import qualified Control.Arrow as Arr
import           Control.Arrow ((&&&), (***), (<<<), (>>>))

-- { Set your test database info here.  Then invoke the 'main'
--   function to run the tests, or just use 'cabal test'.  The test
--   database must already exist and the test user must have
--   permissions to modify it.

connectInfo :: SQL.ConnectInfo
connectInfo =  SQL.ConnectInfo { SQL.connectHost = "localhost"
                               , SQL.connectPort = 25433
                               , SQL.connectUser = "tom"
                               , SQL.connectPassword = "tom"
                               , SQL.connectDatabase = "opaleye_test" }

-- }

{-

Status
======

The tests here are very superficial and pretty much the bare mininmum
that needs to be tested.


Future
======

The overall approach to testing should probably go as follows.

1. Test all individual units of functionality by running them on a
   table and checking that they produce the expected result.  This type
   of testing is amenable to the QuickCheck approach if we reimplement
   the individual units of functionality in Haskell.

2. Test that "the denotation is an arrow morphism" is correct.  I
   think in combination with 1. this is all that will be required to
   demonstrate that the library is correct.

   "The denotation is an arrow morphism" means that for each arrow
   operation, the denotation preserves the operation.  If we have

       f :: QueryArr wiresa wiresb

   then [f] should be something like

       [f] :: a -> IO [b]
       f as = runQuery (toValues as >>> f)

   For example, take the operation >>>.  We need to check that

       [f >>> g] = [f] >>> [g]

   for all f and g, where [] means the denotation.  We would also want
   to check that

       [id] = id

   and

       [first f] = first [f]

   I think checking these operations is sufficient because all the
   other QueryArr operations are implemented in terms of them.

   (Here I'm taking a slight liberty as `a -> IO [b]` is not directly
   an arrow, but it could be made one straightforwardly.  (For the laws
   to be satisfied, perhaps we have to assume that the IO actions
   commute.))

   I don't think this type of testing is amenable to QuickCheck.  It
   seems we have to check the properties for arbitrary arrows indexed by
   arbitrary types.  I don't think QuickCheck supports this sort of
   randomised testing.

Note
----

This seems to be equivalent to just reimplementing Opaleye in
Haskell-side terms and comparing the results of queries run in both
ways.

-}

twoIntTable :: String -> T.Table (TableColumn Int, TableColumn Int)
twoIntTable n = T.makeTable (T.Table n ("column1", "column2"))

table1 :: T.Table (TableColumn Int, TableColumn Int)
table1 = twoIntTable "table1"

table2 :: T.Table (TableColumn Int, TableColumn Int)
table2 = twoIntTable "table2"

table1Q :: Query (Column Int, Column Int)
table1Q = T.queryTable table1

table2Q :: Query (Column Int, Column Int)
table2Q = T.queryTable table2

table1data :: [(Int, Int)]
table1data = [ (1, 100)
             , (1, 100)
             , (1, 200)
             , (2, 300) ]

table2data :: [(Int, Int)]
table2data = [ (1, 100)
             , (3, 400) ]

type Test = SQL.Connection -> IO Bool

testG :: D.Default RQ.QueryRunner wires haskells =>
         Query wires
         -> ([haskells] -> b)
         -> SQL.Connection
         -> IO b
testG q p conn = do
  result <- RQ.runQuery q conn
  return (p result)

testSelect :: Test
testSelect = testG table1Q
             (\r -> L.sort table1data == L.sort r)

testProduct :: Test
testProduct = testG query
                 (\r -> L.sort (A.liftA2 (,) table1data table2data) == L.sort r)
  where query = table1Q &&& table2Q

testRestrict :: Test
testRestrict = testG query
               (\r -> filter ((== 1) . fst) (L.sort table1data) == L.sort r)
  where query = proc () -> do
          t <- table1Q -< ()
          O.restrict -< fst t .== 1
          Arr.returnA -< t

testNum :: Test
testNum = testG query expected
  where query :: Query (Column Int)
        query = proc () -> do
          t <- table1Q -< ()
          Arr.returnA -< op t
        expected = \r -> L.sort (map op table1data) == L.sort r
        op :: Num a => (a, a) -> a
        op (x, y) = abs (x - 5) * signum (x - 4) * (y * y)

testDiv :: Test
testDiv = testG query expected
  where query :: Query (Column Double)
        query = proc () -> do
          t <- Arr.arr (O.doubleOfInt *** O.doubleOfInt) <<< table1Q -< ()
          Arr.returnA -< op t
        expected = \r -> L.sort (map (op . toDoubles) table1data) == L.sort r
        op :: Fractional a => (a, a) -> a
        -- Choosing 0.5 here as it should be exactly representable in
        -- floating point
        op (x, y) = y / x * 0.5
        toDoubles :: (Int, Int) -> (Double, Double)
        toDoubles = fromIntegral *** fromIntegral

-- TODO: need to implement and test case_ returning tuples
testCase :: Test
testCase = testG q (== expected)
  where q :: Query (Column Int)
        q = table1Q >>> proc (i, j) -> do
          Arr.returnA -< O.case_ [(j .== 100, 12), (i .== 1, 21)] 33
        expected :: [Int]
        expected = [12, 12, 21, 33]

allTests :: [Test]
allTests = [testSelect, testProduct, testRestrict, testNum, testDiv, testCase]

main :: IO ()
main = do
  conn <- SQL.connect connectInfo

  results <- mapM ($ conn) allTests

  print results

  let passed = and results

  putStrLn (if passed then "All passed" else "Failure")
  Exit.exitWith (if passed then Exit.ExitSuccess
                           else Exit.ExitFailure 1)
