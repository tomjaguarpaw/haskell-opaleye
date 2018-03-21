{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified QuickCheck

import           Opaleye (Column, Nullable, Query, QueryArr, (.==), (.>))
import qualified Opaleye as O
import qualified Opaleye.Internal.Aggregate as IA

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Range as R
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

import           System.Environment (lookupEnv)

import           Control.Applicative ((<$>), (<*>), (<|>))
import qualified Control.Applicative as A
import qualified Control.Arrow as Arr
import           Control.Arrow ((&&&), (***), (<<<), (>>>))

import           GHC.Int (Int64)

import Test.Hspec

import qualified Configuration.Dotenv as Dotenv

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
        integer c = "\"" ++ c ++ "\"" ++ " " ++ columnType
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
        integer c = "\"" ++ c ++ "\"" ++ " SERIAL"
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

type Test = SpecWith PGS.Connection

testH :: D.Default O.QueryRunner wires haskells =>
         Query wires
         -> ([haskells] -> IO expectation)
         -> PGS.Connection
         -> IO expectation

testH q p conn = do
  result <- O.runQuery conn q
  p result

queryShouldReturnSorted :: (D.Default O.QueryRunner wires haskells, Show haskells, Ord haskells) =>
         Query wires
         -> [haskells]
         -> PGS.Connection
         -> Expectation
queryShouldReturnSorted q expected = testH q (\res -> L.sort res `shouldBe` L.sort expected)

testSelect :: Test
testSelect = it "selects" $ table1Q `queryShouldReturnSorted` table1data

testProduct :: Test
testProduct = it "joins tables" $ query `queryShouldReturnSorted` (A.liftA2 (,) table1data table2data)
  where query = table1Q &&& table2Q

testRestrict :: Test
testRestrict = it "restricts the rows returned" $ query `queryShouldReturnSorted` filter ((== 1) . fst) (L.sort table1data)
  where query = proc () -> do
          t <- table1Q -< ()
          O.restrict -< fst t .== 1
          Arr.returnA -< t

testExists :: Test
testExists = it "restricts the rows returned with EXISTS" $ query `queryShouldReturnSorted` filter ((== 1) . fst) (L.sort table1data)
  where query = proc () -> do
          t <- table1Q -< ()
          () <- O.exists (proc t -> do
                            t' <- table1Q -< ()
                            O.restrict -< fst t' .> fst t) -< t
          Arr.returnA -< t

testNotExists :: Test
testNotExists = it "restricts the rows returned with NOT EXISTS" $ query `queryShouldReturnSorted` filter ((== 2) . fst)  (L.sort table1data)
  where query = proc () -> do
          t <- table1Q -< ()
          () <- O.notExists (proc t -> do
                               t' <- table1Q -< ()
                               O.restrict -< fst t' .> fst t) -< t
          Arr.returnA -< t

testIn :: Test
testIn = it "restricts values to a range" $ query `queryShouldReturnSorted` filter (flip elem [100, 200] . snd) (L.sort table1data)
  where query = proc () -> do
          t <- table1Q -< ()
          O.restrict -< O.in_ [O.pgInt4 100, O.pgInt4 200] (snd t)
          O.restrict -< O.not (O.in_ [] (fst t)) -- Making sure empty lists work.
          Arr.returnA -< t

testNum :: Test
testNum = it "" $ query `queryShouldReturnSorted` (map op table1data)
  where query :: Query (Column O.PGInt4)
        query = proc () -> do
          t <- table1Q -< ()
          Arr.returnA -< op t
        op :: Num a => (a, a) -> a
        op (x, y) = abs (x - 5) * signum (x - 4) * (y * y + 1)

testDiv :: Test
testDiv = it "" $ query `queryShouldReturnSorted` (map (op . toDoubles) table1data)
  where query :: Query (Column O.PGFloat8)
        query = proc () -> do
          t <- Arr.arr (O.doubleOfInt *** O.doubleOfInt) <<< table1Q -< ()
          Arr.returnA -< op t
        op :: Fractional a => (a, a) -> a
        -- Choosing 0.5 here as it should be exactly representable in
        -- floating point
        op (x, y) = y / x * 0.5
        toDoubles :: (Int, Int) -> (Double, Double)
        toDoubles = fromIntegral *** fromIntegral

-- TODO: need to implement and test case_ returning tuples
testCase :: Test
testCase = it "" $ q `queryShouldReturnSorted` expected
  where q :: Query (Column O.PGInt4)
        q = table1Q >>> proc (i, j) -> do
          Arr.returnA -< O.case_ [(j .== 100, 12), (i .== 1, 21)] 33
        expected :: [Int]
        expected = [12, 12, 21, 33]

-- This tests case_ with an empty list of cases, to make sure it generates valid
-- SQL.
testCaseEmpty :: Test
testCaseEmpty = it "" $ q `queryShouldReturnSorted` expected
  where q :: Query (Column O.PGInt4)
        q = table1Q >>> proc _ ->
          Arr.returnA -< O.case_ [] 33
        expected :: [Int]
        expected = [33, 33, 33, 33]

testDistinct :: Test
testDistinct = it "" $ O.distinct table1Q `queryShouldReturnSorted` (L.nub table1data)

-- FIXME: the unsafeCoerceColumn is currently needed because the type
-- changes required for aggregation are not currently dealt with by
-- Opaleye.
aggregateCoerceFIXME :: QueryArr (Column O.PGInt4) (Column O.PGInt8)
aggregateCoerceFIXME = Arr.arr aggregateCoerceFIXME'

aggregateCoerceFIXME' :: Column a -> Column O.PGInt8
aggregateCoerceFIXME' = O.unsafeCoerceColumn

testAggregate :: Test
testAggregate = it "" $ (Arr.second aggregateCoerceFIXME
                        <<< O.aggregate (PP.p2 (O.groupBy, O.sum))
                                           table1Q) `queryShouldReturnSorted` [(1, 400) :: (Int, Int64), (2, 300)]

testAggregate0 :: Test
testAggregate0 = it "" $ (Arr.second aggregateCoerceFIXME
                        <<< O.aggregate (PP.p2 (O.sum, O.sum))
                                        (O.keepWhen (const (O.pgBool False))
                                         <<< table1Q)) `queryShouldReturnSorted` ([] :: [(Int, Int64)])

testAggregateFunction :: Test
testAggregateFunction = it "" $ (Arr.second aggregateCoerceFIXME
                        <<< O.aggregate (PP.p2 (O.groupBy, O.sum))
                                        (fmap (\(x, y) -> (x + 1, y)) table1Q))
                      `queryShouldReturnSorted` [(2, 400) :: (Int, Int64), (3, 300)]

testAggregateProfunctor :: Test
testAggregateProfunctor = it "" $ q `queryShouldReturnSorted` [(1, 1200) :: (Int, Int64), (2, 300)]
  where q = O.aggregate (PP.p2 (O.groupBy, countsum)) table1Q
        countsum = P.dimap (\x -> (x,x))
                           (\(x, y) -> aggregateCoerceFIXME' x * y)
                           (PP.p2 (O.sum, O.count))

testStringArrayAggregate :: Test
testStringArrayAggregate = it "" $ q `queryShouldReturnSorted` [(map fst table6data, minimum (map snd table6data))]
  where q = O.aggregate (PP.p2 (O.arrayAgg, O.min)) table6Q

testStringAggregate :: Test
testStringAggregate = it "" $ q `queryShouldReturnSorted` expected
  where q = O.aggregate (PP.p2 ((O.stringAgg . O.pgString) "_", O.groupBy)) table6Q
        expected = [(
          (foldl1 (\x y -> x ++ "_" ++ y) . map fst) table6data ,
          head (map snd table6data))]

-- | Using aggregateOrdered applies the ordering to all aggregates.

testStringArrayAggregateOrdered :: Test
testStringArrayAggregateOrdered = it "" $ q `queryShouldReturnSorted` expected
  where q = O.aggregateOrdered (O.asc snd) (PP.p2 (O.arrayAgg, O.stringAgg . O.pgString $ ",")) table7Q
        expected = [( map fst sortedData
                      , L.intercalate "," . map snd $ sortedData
                      )
                     ]
        sortedData = L.sortBy (Ord.comparing snd) table7data

-- | Using orderAggregate you can apply different orderings to
-- different aggregates.

testMultipleAggregateOrdered :: Test
testMultipleAggregateOrdered = it "" $ q `queryShouldReturnSorted` expected
  where q = O.aggregate ((,) <$> IA.orderAggregate (O.asc snd)
                                                   (P.lmap fst O.arrayAgg)
                             <*> IA.orderAggregate (O.desc snd)
                                                   (P.lmap snd (O.stringAgg . O.pgString $ ","))
                        ) table7Q
        expected = [( map fst . L.sortBy (Ord.comparing snd) $ table7data
                      , L.intercalate "," . map snd . L.sortBy (Ord.comparing (Ord.Down . snd)) $ table7data
                      )
                     ]

-- | Applying an order to an ordered aggregate overwrites the old
-- order, just like with ordered queries.
--
testOverwriteAggregateOrdered :: Test
testOverwriteAggregateOrdered = it "" $ q `queryShouldReturnSorted` expected
  where q = O.aggregate ( IA.orderAggregate (O.asc snd)
                        . IA.orderAggregate (O.desc snd)
                        $ PP.p2 (O.arrayAgg, O.max)
                        ) table7Q
        expected = [( map fst (L.sortBy (Ord.comparing snd) table7data)
                      , maximum (map snd table7data)
                      )
                     ]

testCountRows0 :: Test
testCountRows0 = it "" $ q `queryShouldReturnSorted` [0 :: Int64]
  where q        = O.countRows (O.keepWhen (const (O.pgBool False)) <<< table7Q)

testCountRows3 :: Test
testCountRows3 = it "" $ q `queryShouldReturnSorted` [3 :: Int64]
  where q        = O.countRows table7Q

queryShouldReturnSortBy :: O.Order (Column O.PGInt4, Column O.PGInt4)
                -> ((Int, Int) -> (Int, Int) -> Ordering)
                -> (PGS.Connection -> Expectation)
queryShouldReturnSortBy orderQ order = testH (O.orderBy orderQ table1Q)
                                  (L.sortBy order table1data `shouldBe`)

testOrderBy :: Test
testOrderBy = it "" $ queryShouldReturnSortBy (O.desc snd)
                           (flip (Ord.comparing snd))

testOrderBy2 :: Test
testOrderBy2 = it "" $ queryShouldReturnSortBy (O.desc fst <> O.asc snd)
                            (flip (Ord.comparing fst) <> Ord.comparing snd)

testOrderBySame :: Test
testOrderBySame = it "" $ queryShouldReturnSortBy (O.desc fst <> O.asc fst)
                               (flip (Ord.comparing fst) <> Ord.comparing fst)

testOrderExact :: Test
testOrderExact = it "" $ testH (O.orderBy (O.exact cols snd) table1Q) (result `shouldBe`)
  where cols   = map O.constant [300,200::Int]
        result = [ (2::Int, 300::Int)
                 , (1, 200)
                 , (1, 100)
                 , (1, 100)
                 ]

limitOrderShouldMatch :: (Query (Column O.PGInt4, Column O.PGInt4) -> Query (Column O.PGInt4, Column O.PGInt4))
           -> ([(Int, Int)] -> [(Int, Int)]) -> (PGS.Connection -> Expectation)
limitOrderShouldMatch olQ ol = testH (olQ (orderQ table1Q))
                       (ol (order table1data) `shouldBe`)
  where orderQ = O.orderBy (O.desc snd)
        order = L.sortBy (flip (Ord.comparing snd))

testLimit :: Test
testLimit = it "" $ limitOrderShouldMatch (O.limit 2) (take 2)

testOffset :: Test
testOffset = it "" $ limitOrderShouldMatch (O.offset 2) (drop 2)

testLimitOffset :: Test
testLimitOffset = it "" $ limitOrderShouldMatch (O.limit 2 . O.offset 2) (take 2 . drop 2)

testOffsetLimit :: Test
testOffsetLimit = it "" $ limitOrderShouldMatch (O.offset 2 . O.limit 2) (drop 2 . take 2)

testDistinctAndAggregate :: Test
testDistinctAndAggregate = it "" $ q `queryShouldReturnSorted` expectedResult
  where q = O.distinct table1Q
            &&& (Arr.second aggregateCoerceFIXME
                 <<< O.aggregate (PP.p2 (O.groupBy, O.sum)) table1Q)
        expectedResult = A.liftA2 (,) (L.nub table1data)
                                      [(1 :: Int, 400 :: Int64), (2, 300)]

one :: Query (Column O.PGInt4)
one = Arr.arr (const (1 :: Column O.PGInt4))

-- The point of the "double" tests is to ensure that we do not
-- introduce name clashes in the operations which create new column names
testDoubleH :: (Show haskells, Eq haskells, D.Default O.QueryRunner columns haskells) =>
               (QueryArr () (Column O.PGInt4) -> QueryArr () columns) -> [haskells]
               -> (PGS.Connection -> Expectation)
testDoubleH q expected1 = testH (q one &&& q one) (`shouldBe` expected2)
  where expected2 = A.liftA2 (,) expected1 expected1

testDoubleDistinct :: Test
testDoubleDistinct = it "" $ testDoubleH O.distinct [1 :: Int]

testDoubleAggregate :: Test
testDoubleAggregate = it "" $ testDoubleH (O.aggregate O.count) [1 :: Int64]

testDoubleLeftJoin :: Test
testDoubleLeftJoin = it "" $ testDoubleH lj [(1 :: Int, Just (1 :: Int))]
  where lj :: Query (Column O.PGInt4)
          -> Query (Column O.PGInt4, Column (Nullable O.PGInt4))
        lj q = O.leftJoin q q (uncurry (.==))

testDoubleValues :: Test
testDoubleValues = it "" $ testDoubleH v [1 :: Int]
  where v :: Query (Column O.PGInt4) -> Query (Column O.PGInt4)
        v _ = O.values [1]

testDoubleUnionAll :: Test
testDoubleUnionAll = it "" $ testDoubleH u [1 :: Int, 1]
  where u q = q `O.unionAll` q

aLeftJoin :: Query ((Column O.PGInt4, Column O.PGInt4),
                    (Column (Nullable O.PGInt4), Column (Nullable O.PGInt4)))
aLeftJoin = O.leftJoin table1Q table3Q (\(l, r) -> fst l .== fst r)

testLeftJoin :: Test
testLeftJoin = it "" $ testH aLeftJoin (`shouldBe` expected)
  where expected :: [((Int, Int), (Maybe Int, Maybe Int))]
        expected = [ ((1, 100), (Just 1, Just 50))
                   , ((1, 100), (Just 1, Just 50))
                   , ((1, 200), (Just 1, Just 50))
                   , ((2, 300), (Nothing, Nothing)) ]

testLeftJoinNullable :: Test
testLeftJoinNullable = it "" $ testH q (`shouldBe` expected)
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

testLeftJoinF :: Test
testLeftJoinF = it "" $ testH q (`shouldBe` expected)
  where q = O.leftJoinF (,)
                        (\x -> (x, (-1, -2)))
                        (\l r -> fst l .== fst r)
                        table1Q
                        table3Q

        expected :: [((Int, Int), (Int, Int))]
        expected = [ ((1, 100), (1, 50))
                   , ((1, 100), (1, 50))
                   , ((1, 200), (1, 50))
                   , ((2, 300), (-1, -2)) ]

testThreeWayProduct :: Test
testThreeWayProduct = it "" $ testH q (`shouldBe` expected)
  where q = A.liftA3 (,,) table1Q table2Q table3Q
        expected = A.liftA3 (,,) table1data table2data table3data

testValues :: Test
testValues = it "" $ testH (O.values values) (values' `shouldBe`)
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
testValuesEmpty = it "" $ testH (O.values values) (values' `shouldBe`)
  where values :: [Column O.PGInt4]
        values = []
        values' :: [Int]
        values' = []

testUnionAll :: Test
testUnionAll = it "" $  (table1Q `O.unionAll` table2Q) `queryShouldReturnSorted` (table1data ++ table2data)

testTableFunctor :: Test
testTableFunctor = it "" $ testH (O.queryTable table1F) (result `shouldBe`)
  where result = fmap (\(col1, col2) -> (col1 + col2, col1 - col2)) table1data

-- TODO: This is getting too complicated
testUpdate :: Test
testUpdate = it "" $ \conn -> do
  _ <- O.runUpdate conn table4 update cond
  result <- runQueryTable4 conn
  result `shouldBe` expected

  _ <- O.runDelete conn table4 condD
  resultD <- runQueryTable4 conn
  resultD `shouldBe` expectedD

  returned <- O.runInsertManyReturning conn table4 insertT returning
  _ <- O.runInsertMany conn table4 insertTMany
  resultI <- runQueryTable4 conn

  resultI `shouldBe` expectedI
  returned `shouldBe` expectedR

  where update (x, y) = (x + y, x - y)
        cond (_, y) = y .> 15
        condD (x, _) = x .> 20
        expected :: [(Int, Int)]
        expected = [ (1, 10)
                   , (22, -18)]
        expectedD :: [(Int, Int)]
        expectedD = [(1, 10)]
        runQueryTable4 conn = O.runQuery conn (O.queryTable table4)

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
testKeywordColNames = it "" $ \conn -> do
  let q :: IO [(Int, Int)]
      q = O.runQuery conn (O.queryTable tableKeywordColNames)
  _ <- q
  True `shouldBe` True

testInsertSerial :: Test
testInsertSerial = it "" $ \conn -> do
  _ <- O.runInsert conn table5 (Just 10, Just 20)
  _ <- O.runInsert conn table5 (Just 30, Nothing)
  _ <- O.runInsert conn table5 (Nothing, Nothing)
  _ <- O.runInsert conn table5 (Nothing, Just 40)

  resultI <- O.runQuery conn (O.queryTable table5)

  resultI `shouldBe` expected

  where expected :: [(Int, Int)]
        expected = [ (10, 20)
                   , (30, 1)
                   , (1, 2)
                   , (2, 40) ]

testInQuery :: Test
testInQuery = it "" $ \conn -> do
  let q (x, e) = testH (O.inQuery x (O.queryTable table1)) (`shouldBe` [e]) conn

  mapM_ (q . (\x ->      (x,        True)))  table1dataG
  mapM_ (q . (\(x, y) -> ((x, y+1), False))) table1dataG

  -- and r && and s `shouldBe` True

testAtTimeZone :: Test
testAtTimeZone = it "" $ testH (A.pure (O.timestamptzAtTimeZone t (O.pgString "CET"))) (`shouldBe` [t'])
  where t = O.pgUTCTime (Time.UTCTime d (Time.secondsToDiffTime 3600))
        t' = Time.LocalTime d (Time.TimeOfDay 2 0 0)
        d = Time.fromGregorian 2015 1 1

testArrayLiterals :: Test
testArrayLiterals = it "" $ testH (A.pure $ O.pgArray O.pgInt4 vals) (`shouldBe` [vals])
  where vals = [1,2,3]

-- This test fails without the explicit cast in pgArray since postgres
-- can't determine the type of the array.

testEmptyArray :: Test
testEmptyArray = it "" $ testH (A.pure $ O.pgArray O.pgInt4 []) (`shouldBe` [[] :: [Int]])

-- This test fails without the explicit cast in pgArray since postgres
-- defaults the numbers to 'integer' but postgresql-simple expects 'float8'.

testFloatArray :: Test
testFloatArray = it "" $ testH (A.pure $ O.pgArray O.pgDouble doubles) (`shouldBe` [doubles])
  where
    doubles = [1 :: Double, 2]

testArrayIndex :: Test
testArrayIndex = it "correctly indexes an array" $
  testH (A.pure $ O.pgArray O.pgInt4 [5,6,7] `O.index` O.pgInt4 3)
        (`shouldBe` ([Just 7] :: [Maybe Int]))

testArrayIndexOOB :: Test
testArrayIndexOOB = it "returns Nothing when the index is out of bounds" $
  testH (A.pure $ O.pgArray O.pgInt4 [5,6,7] `O.index` O.pgInt4 8)
        (`shouldBe` ([Nothing] :: [Maybe Int]))

type JsonTest a = SpecWith (Query (Column a) -> PGS.Connection -> Expectation)
-- Test opaleye's equivalent of c1->'c'
testJsonGetFieldValue :: (O.PGIsJson a, O.QueryRunnerColumnDefault a Json.Value) => Query (Column a) -> Test
testJsonGetFieldValue dataQuery = it "" $ testH q (`shouldBe` expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.pgStrictText "c"
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number $ fromInteger 21]

-- Test opaleye's equivalent of c1->>'c'
testJsonGetFieldText :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetFieldText dataQuery = it "" $ testH q (`shouldBe` expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..->> O.pgStrictText "c"
        expected :: [Maybe T.Text]
        expected = [Just "21"]

-- Special Test for Github Issue #350 : https://github.com/tomjaguarpaw/haskell-opaleye/issues/350
testRestrictWithJsonOp :: (O.PGIsJson a) => Query (Column a) -> Test
testRestrictWithJsonOp dataQuery = it "restricts the rows returned by checking equality with a value extracted using JSON operator" $ testH query (`shouldBe` table8data)
  where query = dataQuery >>> proc col1 -> do
          t <- table8Q -< ()
          O.restrict -< (O.toNullable col1 O..->> O.pgStrictText "c") .== O.toNullable (O.pgStrictText "21")
          Arr.returnA -< t

-- Test opaleye's equivalent of c1->'a'->2
testJsonGetArrayValue :: (O.PGIsJson a, O.QueryRunnerColumnDefault a Json.Value) => Query (Column a) -> Test
testJsonGetArrayValue dataQuery = it "" $ testH q (`shouldBe` expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.pgStrictText "a" O..-> O.pgInt4 2
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number $ fromInteger 30]

-- Test opaleye's equivalent of c1->'a'->>2
testJsonGetArrayText :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetArrayText dataQuery = it "" $ testH q (`shouldBe` expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.pgStrictText "a" O..->> O.pgInt4 2
        expected :: [Maybe T.Text]
        expected = [Just "30"]

-- Test opaleye's equivalent of c1->>'missing'
-- Note that the missing field does not exist.
testJsonGetMissingField :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetMissingField dataQuery = it "" $ testH q (`shouldBe` expected)
  where q = dataQuery >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..->> O.pgStrictText "missing"
        expected :: [Maybe T.Text]
        expected = [Nothing]

-- Test opaleye's equivalent of c1#>'{b,x}'
testJsonGetPathValue :: (O.PGIsJson a, O.QueryRunnerColumnDefault a Json.Value) => Query (Column a) -> Test
testJsonGetPathValue dataQuery = it "" $ testH q (`shouldBe` expected)
  where q = dataQuery >>> proc c1 -> do
              Arr.returnA -< O.toNullable c1 O..#> O.pgArray O.pgStrictText ["b", "x"]
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number $ fromInteger 42]

-- Test opaleye's equivalent of c1#>>'{b,x}'
testJsonGetPathText :: (O.PGIsJson a) => Query (Column a) -> Test
testJsonGetPathText dataQuery = it "" $ testH q (`shouldBe` expected)
  where q = dataQuery >>> proc c1 -> do
              Arr.returnA -< O.toNullable c1 O..#>> O.pgArray O.pgStrictText ["b", "x"]
        expected :: [Maybe T.Text]
        expected = [Just "42"]

-- Test opaleye's equivalent of c1 @> '{"c":21}'::jsonb
testJsonbRightInLeft :: Test
testJsonbRightInLeft = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..@> O.pgJSONB "{\"c\":21}"

-- Test opaleye's equivalent of '{"c":21}'::jsonb <@ c1
testJsonbLeftInRight :: Test
testJsonbLeftInRight = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< O.pgJSONB "{\"c\":21}" O..<@ c1

-- Test opaleye's equivalent of c1 ? 'b'
testJsonbContains :: Test
testJsonbContains = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..? O.pgStrictText "c"

-- Test opaleye's equivalent of c1 ? 'missing'
-- Note that the missing field does not exist.
testJsonbContainsMissing :: Test
testJsonbContainsMissing = it "" $ testH q (`shouldBe` [False])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..? O.pgStrictText "missing"

-- Test opaleye's equivalent of c1 ?| array['b', 'missing']
testJsonbContainsAny :: Test
testJsonbContainsAny = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..?| O.pgArray O.pgStrictText ["b", "missing"]

-- Test opaleye's equivalent of c1 ?& array['a', 'b', 'c']
testJsonbContainsAll :: Test
testJsonbContainsAll = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..?& O.pgArray O.pgStrictText ["a", "b", "c"]

testRangeOverlap :: Test
testRangeOverlap = it "generates overlap" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Column (O.PGRange O.PGInt4)
        range a b = O.pgRange O.pgInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure $ (range 3 7) `O.overlap` (range 4 12)

testRangeDateOverlap :: Test
testRangeDateOverlap = it "generates time overlap" $ \conn -> do
    let date       = Time.fromGregorian 2015 1 1
        now        = Time.UTCTime date (Time.secondsToDiffTime 3600)
        later      = Time.addUTCTime 10 now
        range1     = O.pgRange O.pgUTCTime (R.Inclusive now) (R.Exclusive later)
        range2     = O.pgRange O.pgUTCTime R.NegInfinity R.PosInfinity
        rangeNow   = O.pgRange O.pgUTCTime (R.Inclusive now) (R.Inclusive now)
        qOverlap r = A.pure $ r `O.overlap` rangeNow
    testH (qOverlap range1) (`shouldBe` [True]) conn
    testH (qOverlap range2) (`shouldBe` [True]) conn

testRangeLeftOf :: Test
testRangeLeftOf = it "generates 'left of'" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Column (O.PGRange O.PGInt4)
        range a b = O.pgRange O.pgInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure $ (range 1 10) O..<< (range 100 110)

testRangeRightOf :: Test
testRangeRightOf = it "generates 'right of'" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Column (O.PGRange O.PGInt4)
        range a b = O.pgRange O.pgInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure $ (range 50 60) O..>> (range 20 30)

testRangeRightExtension :: Test
testRangeRightExtension = it "generates right extension" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Column (O.PGRange O.PGInt4)
        range a b = O.pgRange O.pgInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure $ (range 1 20) O..&< (range 18 20)

testRangeLeftExtension :: Test
testRangeLeftExtension = it "generates left extension" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Column (O.PGRange O.PGInt4)
        range a b = O.pgRange O.pgInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure $ (range 7 20) O..&> (range 5 10)

testRangeAdjacency :: Test
testRangeAdjacency = it "generates adjacency" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Column (O.PGRange O.PGInt4)
        range a b = O.pgRange O.pgInt4 (R.Inclusive a) (R.Exclusive b)
        q = A.pure $ (range 1 2) O..-|- (range 2 3)

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

main :: IO ()
main = do
  let envVarName = "POSTGRES_CONNSTRING"

  connectStringEnvVar <- lookupEnv envVarName

  connectStringDotEnv <- do vars <- Dotenv.parseFile ".env"
                            return (lookup envVarName vars)
                         `Dotenv.onMissingFile`
                         return Nothing

  let connectString = connectStringEnvVar <|> connectStringDotEnv

  conn <- maybe
    (fail ("Set " ++ envVarName ++ " environment variable\n"
           ++ "For example " ++ envVarName ++ "='user=tom dbname=opaleye_test "
           ++ "host=localhost port=25433 password=tom'"))
    (PGS.connectPostgreSQL . String.fromString)
    connectString

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

  hspec $ do
    before (return conn) $ do
      describe "core dsl?" $ do
        testSelect
        testProduct
        testRestrict
        testExists
        testNotExists
        testIn
        testNum
        testDiv
      describe "cases" $ do
        testCase
        testCaseEmpty
      describe "aggregate" $ do
        testAggregate
        testAggregate0
        testAggregateFunction
        testAggregateProfunctor
        testStringArrayAggregate
        testStringAggregate
        testOverwriteAggregateOrdered
        testMultipleAggregateOrdered
        testStringArrayAggregateOrdered
        testDistinctAndAggregate
        testDoubleAggregate
      describe "distinct" $ do
        testDistinct
      describe "order" $ do
        testOrderBy
        testOrderBy2
        testOrderBySame
        testOrderExact
      describe "count" $ do
        testCountRows0
        testCountRows3
      describe "limit" $ do
        testLimit
        testOffset
        testLimitOffset
        testOffsetLimit
      describe "double" $ do
        testDoubleDistinct
        testDoubleLeftJoin
        testDoubleValues
        testDoubleUnionAll
      describe "arrays" $ do
        testArrayLiterals
        testEmptyArray
        testFloatArray
        testArrayIndex
        testArrayIndexOOB
      describe "joins" $ do
        testLeftJoin
        testLeftJoinNullable
        testThreeWayProduct
        testLeftJoinF
      describe "json" $ do
        testJsonGetFieldValue   table8Q
        testJsonGetFieldText    table8Q
        testJsonGetMissingField table8Q
        testJsonGetArrayValue   table8Q
        testJsonGetArrayText    table8Q
        testJsonGetPathValue    table8Q
        testJsonGetPathText     table8Q
        testRestrictWithJsonOp  table8Q
      describe "uncat" $ do
        testKeywordColNames
        testInsertSerial
        testInQuery
        testAtTimeZone
        testUnionAll
        testTableFunctor
        testValues
        testValuesEmpty
        testUpdate
      describe "range" $ do
        testRangeOverlap
        testRangeDateOverlap
        testRangeLeftOf
        testRangeRightOf
        testRangeRightExtension
        testRangeLeftExtension
        testRangeAdjacency
