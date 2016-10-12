{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified QuickCheck

import           Opaleye (Column, Nullable, Query, QueryArr, (.==), (.>))
import qualified Opaleye as O
import qualified Opaleye.Internal.Aggregate as IA

import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor as P
import qualified Data.Ord as Ord
import qualified Data.List as L
import           Data.Monoid ((<>))
import qualified Data.String as String
import qualified Data.Time   as Time
import qualified Data.Aeson as Json
import qualified Data.Text as T

import qualified System.Exit as Exit
import qualified System.Environment as Environment

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Applicative as A
import qualified Control.Arrow as Arr
import           Control.Arrow ((&&&), (***), (<<<), (>>>))

import           GHC.Int (Int64)

-- { Set your test database info here.  Then invoke the 'main'
--   function to run the tests, or just use 'cabal test'.  The test
--   database must already exist and the test user must have
--   permissions to modify it.

connectInfo :: PGS.ConnectInfo
connectInfo =  PGS.ConnectInfo { PGS.connectHost = "localhost"
                               , PGS.connectPort = 25433
                               , PGS.connectUser = "tom"
                               , PGS.connectPassword = "tom"
                               , PGS.connectDatabase = "opaleye_test" }

connectInfoTravis :: PGS.ConnectInfo
connectInfoTravis =  PGS.ConnectInfo { PGS.connectHost = "localhost"
                                     , PGS.connectPort = 5432
                                     , PGS.connectUser = "postgres"
                                     , PGS.connectPassword = ""
                                     , PGS.connectDatabase = "opaleye_test" }

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

twoIntTable :: String
            -> O.Table (Column O.PGInt4, Column O.PGInt4) (Column O.PGInt4, Column O.PGInt4)
twoIntTable n = O.Table n (PP.p2 (O.required "column1", O.required "column2"))

table1 :: O.Table (Column O.PGInt4, Column O.PGInt4) (Column O.PGInt4, Column O.PGInt4)
table1 = twoIntTable "table1"

table1F :: O.Table (Column O.PGInt4, Column O.PGInt4) (Column O.PGInt4, Column O.PGInt4)
table1F = fmap (\(col1, col2) -> (col1 + col2, col1 - col2)) table1

-- This is implicitly testing our ability to handle upper case letters in table names.
table2 :: O.Table (Column O.PGInt4, Column O.PGInt4) (Column O.PGInt4, Column O.PGInt4)
table2 = twoIntTable "TABLE2"

table3 :: O.Table (Column O.PGInt4, Column O.PGInt4) (Column O.PGInt4, Column O.PGInt4)
table3 = twoIntTable "table3"

table4 :: O.Table (Column O.PGInt4, Column O.PGInt4) (Column O.PGInt4, Column O.PGInt4)
table4 = twoIntTable "table4"

table5 :: O.Table (Maybe (Column O.PGInt4), Maybe (Column  O.PGInt4))
                  (Column O.PGInt4, Column O.PGInt4)
table5 = O.TableWithSchema "public" "table5" (PP.p2 (O.optional "column1", O.optional "column2"))

table6 :: O.Table (Column O.PGText, Column O.PGText) (Column O.PGText, Column O.PGText)
table6 = O.Table "table6" (PP.p2 (O.required "column1", O.required "column2"))

table7 :: O.Table (Column O.PGText, Column O.PGText) (Column O.PGText, Column O.PGText)
table7 = O.Table "table7" (PP.p2 (O.required "column1", O.required "column2"))

table8 :: O.Table (Column O.PGJson) (Column O.PGJson)
table8 = O.Table "table8" (O.required "column1")

table9 :: O.Table (Column O.PGJsonb) (Column O.PGJsonb)
table9 = O.Table "table9" (O.required "column1")

tableKeywordColNames :: O.Table (Column O.PGInt4, Column O.PGInt4)
                                (Column O.PGInt4, Column O.PGInt4)
tableKeywordColNames = O.Table "keywordtable" (PP.p2 (O.required "column", O.required "where"))

table1Q :: Query (Column O.PGInt4, Column O.PGInt4)
table1Q = O.queryTable table1

table2Q :: Query (Column O.PGInt4, Column O.PGInt4)
table2Q = O.queryTable table2

table3Q :: Query (Column O.PGInt4, Column O.PGInt4)
table3Q = O.queryTable table3

table6Q :: Query (Column O.PGText, Column O.PGText)
table6Q = O.queryTable table6

table7Q :: Query (Column O.PGText, Column O.PGText)
table7Q = O.queryTable table7

table8Q :: Query (Column O.PGJson)
table8Q = O.queryTable table8

table9Q :: Query (Column O.PGJsonb)
table9Q = O.queryTable table9

table1dataG :: Num a => [(a, a)]
table1dataG = [ (1, 100)
              , (1, 100)
              , (1, 200)
              , (2, 300) ]

table1data :: [(Int, Int)]
table1data = table1dataG

table1columndata :: [(Column O.PGInt4, Column O.PGInt4)]
table1columndata = table1dataG

table2dataG :: Num a => [(a, a)]
table2dataG = [ (1, 100)
              , (3, 400) ]

table2data :: [(Int, Int)]
table2data = table2dataG

table2columndata :: [(Column O.PGInt4, Column O.PGInt4)]
table2columndata = table2dataG

table3dataG :: Num a => [(a, a)]
table3dataG = [ (1, 50) ]

table3data :: [(Int, Int)]
table3data = table3dataG

table3columndata :: [(Column O.PGInt4, Column O.PGInt4)]
table3columndata = table3dataG

table4dataG :: Num a => [(a, a)]
table4dataG = [ (1, 10)
              , (2, 20) ]

table4data :: [(Int, Int)]
table4data = table4dataG

table4columndata :: [(Column O.PGInt4, Column O.PGInt4)]
table4columndata = table4dataG

table6data :: [(String, String)]
table6data = [("xy", "a"), ("z", "a"), ("more text", "a")]

table6columndata :: [(Column O.PGText, Column O.PGText)]
table6columndata = map (\(column1, column2) -> (O.pgString column1, O.pgString column2)) table6data

table7data :: [(String, String)]
table7data = [("foo", "c"), ("bar", "a"), ("baz", "b")]

table7columndata :: [(Column O.PGText, Column O.PGText)]
table7columndata = map (O.pgString *** O.pgString) table7data

table8data :: [Json.Value]
table8data = [ Json.object
               [ "a" Json..= ([10,20..100] :: [Int])
               , "b" Json..= Json.object ["x" Json..= (42 :: Int)]
               , "c" Json..= (21 :: Int)
               ]
             ]

table8columndata :: [Column O.PGJson]
table8columndata = map O.pgValueJSON table8data

table9columndata :: [Column O.PGJsonb]
table9columndata = map O.pgValueJSONB table8data

-- We have to quote the table names here because upper case letters in
-- table names are treated as lower case unless the name is quoted!
dropAndCreateTable :: String -> (String, [String]) -> PGS.Query
dropAndCreateTable columnType (t, cols) = String.fromString drop_
  where drop_ = "DROP TABLE IF EXISTS \"public\".\"" ++ t ++ "\";"
                ++ "CREATE TABLE \"public\".\"" ++ t ++ "\""
                ++ " (" ++ commas cols ++ ");"
        integer c = ("\"" ++ c ++ "\"" ++ " " ++ columnType)
        commas = L.intercalate "," . map integer

dropAndCreateTableInt :: (String, [String]) -> PGS.Query
dropAndCreateTableInt = dropAndCreateTable "integer"

dropAndCreateTableText :: (String, [String]) -> PGS.Query
dropAndCreateTableText = dropAndCreateTable "text"

-- We have to quote the table names here because upper case letters in
-- table names are treated as lower case unless the name is quoted!
dropAndCreateTableSerial :: (String, [String]) -> PGS.Query
dropAndCreateTableSerial (t, cols) = String.fromString drop_
  where drop_ = "DROP TABLE IF EXISTS \"public\".\"" ++ t ++ "\";"
                ++ "CREATE TABLE \"public\".\"" ++ t ++ "\""
                ++ " (" ++ commas cols ++ ");"
        integer c = ("\"" ++ c ++ "\"" ++ " SERIAL")
        commas = L.intercalate "," . map integer

dropAndCreateTableJson :: (String, [String]) -> PGS.Query
dropAndCreateTableJson = dropAndCreateTable "json"

dropAndCreateTableJsonb :: (String, [String]) -> PGS.Query
dropAndCreateTableJsonb = dropAndCreateTable "jsonb"

type Table_ = (String, [String])

-- This should ideally be derived from the table definition above
columns2 :: String -> Table_
columns2 t = (t, ["column1", "column2"])

-- This should ideally be derived from the table definition above
tables :: [Table_]
tables = map columns2 ["table1", "TABLE2", "table3", "table4"]
         ++ [("keywordtable", ["column", "where"])]

serialTables :: [Table_]
serialTables = map columns2 ["table5"]

textTables :: [Table_]
textTables = map columns2 ["table6", "table7"]

jsonTables :: [Table_]
jsonTables = [("table8", ["column1"])]

jsonbTables :: [Table_]
jsonbTables = [("table9", ["column1"])]

dropAndCreateDB :: PGS.Connection -> IO ()
dropAndCreateDB conn = do
  mapM_ execute tables
  mapM_ executeTextTable textTables
  mapM_ executeSerial serialTables
  mapM_ executeJson jsonTables
  -- Disabled until Travis supports Postgresql >= 9.4
  -- mapM_ executeJsonb jsonbTables
  where execute = PGS.execute_ conn . dropAndCreateTableInt
        executeTextTable = PGS.execute_ conn . dropAndCreateTableText
        executeSerial = PGS.execute_ conn . dropAndCreateTableSerial
        executeJson = PGS.execute_ conn . dropAndCreateTableJson
        -- executeJsonb = PGS.execute_ conn . dropAndCreateTableJsonb

type Test = PGS.Connection -> IO Bool

testG :: D.Default O.QueryRunner wires haskells =>
         Query wires
         -> ([haskells] -> b)
         -> PGS.Connection
         -> IO b
testG q p conn = do
  result <- O.runQuery conn q
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

testIn :: Test
testIn = testG query expected
  where query = proc () -> do
          t <- table1Q -< ()
          O.restrict -< O.in_ [O.pgInt4 100, O.pgInt4 200] (snd t)
          O.restrict -< O.not (O.in_ [] (fst t)) -- Making sure empty lists work.
          Arr.returnA -< t
        expected = \r ->
          filter (flip elem [100, 200] . snd) (L.sort table1data) == L.sort r

testNum :: Test
testNum = testG query expected
  where query :: Query (Column O.PGInt4)
        query = proc () -> do
          t <- table1Q -< ()
          Arr.returnA -< op t
        expected = \r -> L.sort (map op table1data) == L.sort r
        op :: Num a => (a, a) -> a
        op (x, y) = abs (x - 5) * signum (x - 4) * (y * y + 1)

testDiv :: Test
testDiv = testG query expected
  where query :: Query (Column O.PGFloat8)
        query = proc () -> do
          t <- Arr.arr (O.doubleOfInt *** O.doubleOfInt) <<< table1Q -< ()
          Arr.returnA -< op t
        expected r = L.sort (map (op . toDoubles) table1data) == L.sort r
        op :: Fractional a => (a, a) -> a
        -- Choosing 0.5 here as it should be exactly representable in
        -- floating point
        op (x, y) = y / x * 0.5
        toDoubles :: (Int, Int) -> (Double, Double)
        toDoubles = fromIntegral *** fromIntegral

-- TODO: need to implement and test case_ returning tuples
testCase :: Test
testCase = testG q (== expected)
  where q :: Query (Column O.PGInt4)
        q = table1Q >>> proc (i, j) -> do
          Arr.returnA -< O.case_ [(j .== 100, 12), (i .== 1, 21)] 33
        expected :: [Int]
        expected = [12, 12, 21, 33]

-- This tests case_ with an empty list of cases, to make sure it generates valid
-- SQL.
testCaseEmpty :: Test
testCaseEmpty = testG q (== expected)
  where q :: Query (Column O.PGInt4)
        q = table1Q >>> proc _ ->
          Arr.returnA -< O.case_ [] 33
        expected :: [Int]
        expected = [33, 33, 33, 33]

testDistinct :: Test
testDistinct = testG (O.distinct table1Q)
               (\r -> L.sort (L.nub table1data) == L.sort r)

-- FIXME: the unsafeCoerceColumn is currently needed because the type
-- changes required for aggregation are not currently dealt with by
-- Opaleye.
aggregateCoerceFIXME :: QueryArr (Column O.PGInt4) (Column O.PGInt8)
aggregateCoerceFIXME = Arr.arr aggregateCoerceFIXME'

aggregateCoerceFIXME' :: Column a -> Column O.PGInt8
aggregateCoerceFIXME' = O.unsafeCoerceColumn

testAggregate :: Test
testAggregate = testG (Arr.second aggregateCoerceFIXME
                        <<< O.aggregate (PP.p2 (O.groupBy, O.sum))
                                           table1Q)
                      (\r -> [(1, 400) :: (Int, Int64), (2, 300)] == L.sort r)

testAggregate0 :: Test
testAggregate0 = testG (Arr.second aggregateCoerceFIXME
                        <<< O.aggregate (PP.p2 (O.sum, O.sum))
                                        (O.keepWhen (const (O.pgBool False))
                                         <<< table1Q))
                      (== ([] :: [(Int, Int64)]))

testAggregateFunction :: Test
testAggregateFunction = testG (Arr.second aggregateCoerceFIXME
                        <<< O.aggregate (PP.p2 (O.groupBy, O.sum))
                                        (fmap (\(x, y) -> (x + 1, y)) table1Q))
                      (\r -> [(2, 400) :: (Int, Int64), (3, 300)] == L.sort r)

testAggregateProfunctor :: Test
testAggregateProfunctor = testG q expected
  where q = O.aggregate (PP.p2 (O.groupBy, countsum)) table1Q
        expected r = [(1, 1200) :: (Int, Int64), (2, 300)] == L.sort r
        countsum = P.dimap (\x -> (x,x))
                           (\(x, y) -> aggregateCoerceFIXME' x * y)
                           (PP.p2 (O.sum, O.count))

testStringArrayAggregate :: Test
testStringArrayAggregate = testG q expected
  where q = O.aggregate (PP.p2 (O.arrayAgg, O.min)) table6Q
        expected r = [(map fst table6data, minimum (map snd table6data))] == r

testStringAggregate :: Test
testStringAggregate = testG q expected
  where q = O.aggregate (PP.p2 ((O.stringAgg . O.pgString) "_", O.groupBy)) table6Q
        expected r = [(
          (foldl1 (\x y -> x ++ "_" ++ y) . map fst) table6data ,
          head (map snd table6data))] == r

-- | Using aggregateOrdered applies the ordering to all aggregates.

testStringArrayAggregateOrdered :: Test
testStringArrayAggregateOrdered = testG q expected
  where q = O.aggregateOrdered (O.asc snd) (PP.p2 (O.arrayAgg, O.stringAgg . O.pgString $ ",")) table7Q
        expected r = [( map fst sortedData
                      , L.intercalate "," . map snd $ sortedData
                      )
                     ] == r
        sortedData = L.sortBy (Ord.comparing snd) table7data

-- | Using orderAggregate you can apply different orderings to
-- different aggregates.

testMultipleAggregateOrdered :: Test
testMultipleAggregateOrdered = testG q expected
  where q = O.aggregate ((,) <$> IA.orderAggregate (O.asc snd)
                                                   (P.lmap fst O.arrayAgg)
                             <*> IA.orderAggregate (O.desc snd)
                                                   (P.lmap snd (O.stringAgg . O.pgString $ ","))
                        ) table7Q
        expected r = [( map fst . L.sortBy (Ord.comparing snd) $ table7data
                      , L.intercalate "," . map snd . L.sortBy (Ord.comparing (Ord.Down . snd)) $ table7data
                      )
                     ] == r

-- | Applying an order to an ordered aggregate overwrites the old
-- order, just like with ordered queries.
--
testOverwriteAggregateOrdered :: Test
testOverwriteAggregateOrdered = testG q expected
  where q = O.aggregate ( IA.orderAggregate (O.asc snd)
                        . IA.orderAggregate (O.desc snd)
                        $ PP.p2 (O.arrayAgg, O.max)
                        ) table7Q
        expected r = [( map fst (L.sortBy (Ord.comparing snd) table7data)
                      , maximum (map snd table7data)
                      )
                     ] == r

testCountRows0 :: Test
testCountRows0 = testG q expected
  where q        = O.countRows (O.keepWhen (const (O.pgBool False)) <<< table7Q)
        expected = (== [0 :: Int64])

testCountRows3 :: Test
testCountRows3 = testG q expected
  where q        = O.countRows table7Q
        expected = (== [3 :: Int64])

testOrderByG :: O.Order (Column O.PGInt4, Column O.PGInt4)
                -> ((Int, Int) -> (Int, Int) -> Ordering)
                -> Test
testOrderByG orderQ order = testG (O.orderBy orderQ table1Q)
                                  (L.sortBy order table1data ==)

testOrderBy :: Test
testOrderBy = testOrderByG (O.desc snd)
                           (flip (Ord.comparing snd))

testOrderBy2 :: Test
testOrderBy2 = testOrderByG (O.desc fst <> O.asc snd)
                            (flip (Ord.comparing fst) <> Ord.comparing snd)

testOrderBySame :: Test
testOrderBySame = testOrderByG (O.desc fst <> O.asc fst)
                               (flip (Ord.comparing fst) <> Ord.comparing fst)

testLOG :: (Query (Column O.PGInt4, Column O.PGInt4) -> Query (Column O.PGInt4, Column O.PGInt4))
           -> ([(Int, Int)] -> [(Int, Int)]) -> Test
testLOG olQ ol = testG (olQ (orderQ table1Q))
                       (ol (order table1data) ==)
  where orderQ = O.orderBy (O.desc snd)
        order = L.sortBy (flip (Ord.comparing snd))

testLimit :: Test
testLimit = testLOG (O.limit 2) (take 2)

testOffset :: Test
testOffset = testLOG (O.offset 2) (drop 2)

testLimitOffset :: Test
testLimitOffset = testLOG (O.limit 2 . O.offset 2) (take 2 . drop 2)

testOffsetLimit :: Test
testOffsetLimit = testLOG (O.offset 2 . O.limit 2) (drop 2 . take 2)

testDistinctAndAggregate :: Test
testDistinctAndAggregate = testG q expected
  where q = O.distinct table1Q
            &&& (Arr.second aggregateCoerceFIXME
                 <<< O.aggregate (PP.p2 (O.groupBy, O.sum)) table1Q)
        expected r = L.sort r == L.sort expectedResult
        expectedResult = A.liftA2 (,) (L.nub table1data)
                                      [(1 :: Int, 400 :: Int64), (2, 300)]

one :: Query (Column O.PGInt4)
one = Arr.arr (const (1 :: Column O.PGInt4))

-- The point of the "double" tests is to ensure that we do not
-- introduce name clashes in the operations which create new column names
testDoubleG :: (Eq haskells, D.Default O.QueryRunner columns haskells) =>
               (QueryArr () (Column O.PGInt4) -> QueryArr () columns) -> [haskells]
               -> Test
testDoubleG q expected1 = testG (q one &&& q one) (== expected2)
  where expected2 = A.liftA2 (,) expected1 expected1

testDoubleDistinct :: Test
testDoubleDistinct = testDoubleG O.distinct [1 :: Int]

testDoubleAggregate :: Test
testDoubleAggregate = testDoubleG (O.aggregate O.count) [1 :: Int64]

testDoubleLeftJoin :: Test
testDoubleLeftJoin = testDoubleG lj [(1 :: Int, Just (1 :: Int))]
  where lj :: Query (Column O.PGInt4)
          -> Query (Column O.PGInt4, Column (Nullable O.PGInt4))
        lj q = O.leftJoin q q (uncurry (.==))

testDoubleValues :: Test
testDoubleValues = testDoubleG v [1 :: Int]
  where v :: Query (Column O.PGInt4) -> Query (Column O.PGInt4)
        v _ = O.values [1]

testDoubleUnionAll :: Test
testDoubleUnionAll = testDoubleG u [1 :: Int, 1]
  where u q = q `O.unionAll` q

aLeftJoin :: Query ((Column O.PGInt4, Column O.PGInt4),
                    (Column (Nullable O.PGInt4), Column (Nullable O.PGInt4)))
aLeftJoin = O.leftJoin table1Q table3Q (\(l, r) -> fst l .== fst r)

testLeftJoin :: Test
testLeftJoin = testG aLeftJoin (== expected)
  where expected :: [((Int, Int), (Maybe Int, Maybe Int))]
        expected = [ ((1, 100), (Just 1, Just 50))
                   , ((1, 100), (Just 1, Just 50))
                   , ((1, 200), (Just 1, Just 50))
                   , ((2, 300), (Nothing, Nothing)) ]

testLeftJoinNullable :: Test
testLeftJoinNullable = testG q (== expected)
  where q :: Query ((Column O.PGInt4, Column O.PGInt4),
                    ((Column (Nullable O.PGInt4), Column (Nullable O.PGInt4)),
                     (Column (Nullable O.PGInt4),
                      Column (Nullable O.PGInt4))))
        q = O.leftJoin table3Q aLeftJoin cond

        cond (x, y) = fst x .== fst (fst y)

        expected :: [((Int, Int), ((Maybe Int, Maybe Int), (Maybe Int, Maybe Int)))]
        expected = [ ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 200), (Just 1, Just 50))) ]

testThreeWayProduct :: Test
testThreeWayProduct = testG q (== expected)
  where q = A.liftA3 (,,) table1Q table2Q table3Q
        expected = A.liftA3 (,,) table1data table2data table3data

testValues :: Test
testValues = testG (O.values values) (values' ==)
  where values :: [(Column O.PGInt4, Column O.PGInt4)]
        values = [ (1, 10)
                 , (2, 100) ]
        values' :: [(Int, Int)]
        values' = [ (1, 10)
                  , (2, 100) ]

{- FIXME: does not yet work
testValuesDouble :: Test
testValuesDouble = testG (O.values values) (values' ==)
  where values :: [(Column O.PGInt4, Column O.PGFloat8)]
        values = [ (1, 10.0)
                 , (2, 100.0) ]
        values' :: [(Int, Double)]
        values' = [ (1, 10.0)
                  , (2, 100.0) ]
-}

testValuesEmpty :: Test
testValuesEmpty = testG (O.values values) (values' ==)
  where values :: [Column O.PGInt4]
        values = []
        values' :: [Int]
        values' = []

testUnionAll :: Test
testUnionAll = testG (table1Q `O.unionAll` table2Q)
                     (\r -> L.sort (table1data ++ table2data) == L.sort r)

testTableFunctor :: Test
testTableFunctor = testG (O.queryTable table1F) (result ==)
  where result = fmap (\(col1, col2) -> (col1 + col2, col1 - col2)) table1data

-- TODO: This is getting too complicated
testUpdate :: Test
testUpdate conn = do
  _ <- O.runUpdate conn table4 update cond
  result <- runQueryTable4

  if result /= expected
    then return False
    else do
    _ <- O.runDelete conn table4 condD
    resultD <- runQueryTable4

    if resultD /= expectedD
      then return False
      else do
      returned <- O.runInsertManyReturning conn table4 insertT returning
      _ <- O.runInsertMany conn table4 insertTMany
      resultI <- runQueryTable4

      return ((resultI == expectedI) && (returned == expectedR))

  where update (x, y) = (x + y, x - y)
        cond (_, y) = y .> 15
        condD (x, _) = x .> 20
        expected :: [(Int, Int)]
        expected = [ (1, 10)
                   , (22, -18)]
        expectedD :: [(Int, Int)]
        expectedD = [(1, 10)]
        runQueryTable4 = O.runQuery conn (O.queryTable table4)

        insertT :: [(Column O.PGInt4, Column O.PGInt4)]
        insertT = [(1, 2), (3, 5)]

        insertTMany :: [(Column O.PGInt4, Column O.PGInt4)]
        insertTMany = [(20, 30), (40, 50)]

        expectedI :: [(Int, Int)]
        expectedI = [(1, 10), (1, 2), (3, 5), (20, 30), (40, 50)]
        returning (x, y) = x - y
        expectedR :: [Int]
        expectedR = [-1, -2]

testKeywordColNames :: Test
testKeywordColNames conn = do
  let q :: IO [(Int, Int)]
      q = O.runQuery conn (O.queryTable tableKeywordColNames)
  _ <- q
  return True

testInsertSerial :: Test
testInsertSerial conn = do
  _ <- O.runInsert conn table5 (Just 10, Just 20)
  _ <- O.runInsert conn table5 (Just 30, Nothing)
  _ <- O.runInsert conn table5 (Nothing, Nothing)
  _ <- O.runInsert conn table5 (Nothing, Just 40)

  resultI <- O.runQuery conn (O.queryTable table5)

  return (resultI == expected)

  where expected :: [(Int, Int)]
        expected = [ (10, 20)
                   , (30, 1)
                   , (1, 2)
                   , (2, 40) ]

testInQuery :: Test
testInQuery conn = do
  let q (x, e) = testG (O.inQuery x (O.queryTable table1)) (== [e]) conn

  r <- mapM (q . (\x ->      (x,        True)))  table1dataG
  s <- mapM (q . (\(x, y) -> ((x, y+1), False))) table1dataG

  return (and r && and s)

testAtTimeZone :: Test
testAtTimeZone = testG (A.pure (O.timestamptzAtTimeZone t (O.pgString "CET"))) (== [t'])
  where t = O.pgUTCTime (Time.UTCTime d (Time.secondsToDiffTime 3600))
        t' = Time.LocalTime d (Time.TimeOfDay 2 0 0)
        d = Time.fromGregorian 2015 1 1

testArrayLiterals :: Test
testArrayLiterals = testG (A.pure $ O.pgArray O.pgInt4 vals) (== [vals])
  where vals = [1,2,3]

-- This test fails without the explicit cast in pgArray since postgres
-- can't determine the type of the array.

testEmptyArray :: Test
testEmptyArray = testG (A.pure $ O.pgArray O.pgInt4 []) (== [[] :: [Int]])

-- This test fails without the explicit cast in pgArray since postgres
-- defaults the numbers to 'integer' but postgresql-simple expects 'float8'.

testFloatArray :: Test
testFloatArray = testG (A.pure $ O.pgArray O.pgDouble doubles) (== [doubles])
  where
    doubles = [1 :: Double, 2]

-- Test opaleye's equivalent of c1->'c'
testJsonGetFieldValue :: (O.PGIsJson a, O.QueryRunnerColumnDefault a Json.Value) => Query (Column a) -> Test
testJsonGetFieldValue dataQuery = testG q (== expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.pgStrictText "c"
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number $ fromInteger 21]

-- Test opaleye's equivalent of c1->>'c'
testJsonGetFieldText :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetFieldText dataQuery = testG q (== expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..->> O.pgStrictText "c"
        expected :: [Maybe T.Text]
        expected = [Just "21"]

-- Test opaleye's equivalent of c1->'a'->2
testJsonGetArrayValue :: (O.PGIsJson a, O.QueryRunnerColumnDefault a Json.Value) => Query (Column a) -> Test
testJsonGetArrayValue dataQuery = testG q (== expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.pgStrictText "a" O..-> O.pgInt4 2
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number $ fromInteger 30]

-- Test opaleye's equivalent of c1->'a'->>2
testJsonGetArrayText :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetArrayText dataQuery = testG q (== expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.pgStrictText "a" O..->> O.pgInt4 2
        expected :: [Maybe T.Text]
        expected = [Just "30"]

-- Test opaleye's equivalent of c1->>'missing'
-- Note that the missing field does not exist.
testJsonGetMissingField :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetMissingField dataQuery = testG q (== expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..->> O.pgStrictText "missing"
        expected :: [Maybe T.Text]
        expected = [Nothing]

-- Test opaleye's equivalent of c1#>'{b,x}'
testJsonGetPathValue :: (O.PGIsJson a, O.QueryRunnerColumnDefault a Json.Value) => Query (Column a) -> Test
testJsonGetPathValue dataQuery = testG q (== expected)
  where q = dataQuery >>> proc c1 -> do
              Arr.returnA -< O.toNullable c1 O..#> O.pgArray O.pgStrictText ["b", "x"]
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number $ fromInteger 42]

-- Test opaleye's equivalent of c1#>>'{b,x}'
testJsonGetPathText :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetPathText dataQuery = testG q (== expected)
  where q = dataQuery >>> proc c1 -> do
              Arr.returnA -< O.toNullable c1 O..#>> O.pgArray O.pgStrictText ["b", "x"]
        expected :: [Maybe T.Text]
        expected = [Just "42"]

-- Test opaleye's equivalent of c1 @> '{"c":21}'::jsonb
testJsonbRightInLeft :: Test
testJsonbRightInLeft = testG q (== [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..@> O.pgJSONB "{\"c\":21}"

-- Test opaleye's equivalent of '{"c":21}'::jsonb <@ c1
testJsonbLeftInRight :: Test
testJsonbLeftInRight = testG q (== [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< O.pgJSONB "{\"c\":21}" O..<@ c1

-- Test opaleye's equivalent of c1 ? 'b'
testJsonbContains :: Test
testJsonbContains = testG q (== [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..? O.pgStrictText "c"

-- Test opaleye's equivalent of c1 ? 'missing'
-- Note that the missing field does not exist.
testJsonbContainsMissing :: Test
testJsonbContainsMissing = testG q (== [False])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..? O.pgStrictText "missing"

-- Test opaleye's equivalent of c1 ?| array['b', 'missing']
testJsonbContainsAny :: Test
testJsonbContainsAny = testG q (== [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..?| O.pgArray O.pgStrictText ["b", "missing"]

-- Test opaleye's equivalent of c1 ?& array['a', 'b', 'c']
testJsonbContainsAll :: Test
testJsonbContainsAll = testG q (== [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..?& O.pgArray O.pgStrictText ["a", "b", "c"]

allTests :: [Test]
allTests = [testSelect, testProduct, testRestrict, testNum, testDiv, testCase,
            testDistinct, testAggregate, testAggregate0, testAggregateFunction,
            testAggregateProfunctor, testStringArrayAggregate, testStringAggregate,
            testOrderBy, testOrderBy2, testOrderBySame, testLimit, testOffset,
            testLimitOffset, testOffsetLimit, testDistinctAndAggregate, testIn,
            testDoubleDistinct, testDoubleAggregate, testDoubleLeftJoin,
            testDoubleValues, testDoubleUnionAll,
            testLeftJoin, testLeftJoinNullable, testThreeWayProduct, testValues,
            testValuesEmpty, testUnionAll, testTableFunctor, testUpdate,
            testKeywordColNames, testInsertSerial, testInQuery, testAtTimeZone,
            testStringArrayAggregateOrdered, testMultipleAggregateOrdered,
            testOverwriteAggregateOrdered, testCountRows0, testCountRows3,
            testArrayLiterals, testEmptyArray, testFloatArray, testCaseEmpty,
            testJsonGetFieldValue   table8Q, testJsonGetFieldText  table8Q,
            testJsonGetMissingField table8Q, testJsonGetArrayValue table8Q,
            testJsonGetArrayText    table8Q, testJsonGetPathValue  table8Q,
            testJsonGetPathText     table8Q
            ]

-- Note: these tests are left out of allTests until Travis supports
-- Postgresql >= 9.4
jsonbTests :: [Test]
jsonbTests = [testJsonGetFieldValue  table9Q,testJsonGetFieldText  table9Q,
             testJsonGetMissingField table9Q,testJsonGetArrayValue table9Q,
             testJsonGetArrayText    table9Q,testJsonGetPathValue  table9Q,
             testJsonGetPathText     table9Q,
             testJsonbRightInLeft, testJsonbLeftInRight,
             testJsonbContains, testJsonbContainsMissing,
             testJsonbContainsAny, testJsonbContainsAll
             ]

-- Environment.getEnv throws an exception on missing environment variable!
getEnv :: String -> IO (Maybe String)
getEnv var = do
  environment <- Environment.getEnvironment
  return (lookup var environment)

-- Using an envvar is unpleasant, but it will do for now.
travis :: IO Bool
travis = do
    travis' <- getEnv "TRAVIS"

    return (case travis' of
               Nothing    -> False
               Just "yes" -> True
               Just _     -> False)

main :: IO ()
main = do
  travis' <- travis

  let connectInfo' = if travis' then connectInfoTravis else connectInfo

  conn <- PGS.connect connectInfo'

  dropAndCreateDB conn

  let insert (writeable, columndata) =
        mapM_ (O.runInsert conn writeable) columndata

  mapM_ insert [ (table1, table1columndata)
               , (table2, table2columndata)
               , (table3, table3columndata)
               , (table4, table4columndata) ]
  insert (table6, table6columndata)
  insert (table7, table7columndata)
  insert (table8, table8columndata)
  -- Disabled until Travis supports Postgresql >= 9.4
  -- insert (table9, table9columndata)

  -- Need to run quickcheck after table data has been inserted
  QuickCheck.run conn

  results <- mapM ($ conn) allTests

  print results

  let passed = and results

  putStrLn (if passed then "All passed" else "Failure")
  Exit.exitWith (if passed then Exit.ExitSuccess
                           else Exit.ExitFailure 1)
