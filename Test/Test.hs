{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Configuration.Dotenv             as Dotenv
import           Control.Applicative              ((<$>), (<*>), (<|>))
import qualified Control.Applicative              as A
import           Control.Arrow                    ((&&&), (***), (<<<), (>>>))
import qualified Control.Arrow                    as Arr
import           Control.Monad                    (guard)
import qualified Data.Aeson                       as Json
import qualified Data.Function                    as F
import           Data.Int (Int32)
import qualified Data.List                        as L
import           Data.Monoid                      ((<>))
import qualified Data.Ord                         as Ord
import qualified Data.Profunctor                  as P
import qualified Data.Profunctor.Product          as PP
import qualified Data.Profunctor.Product.Default  as D
import qualified Data.String                      as String
import qualified Data.ByteString                  as SBS
import qualified Data.Text                        as T
import qualified Data.Time.Compat                 as Time
import qualified Data.Time.Clock.POSIX.Compat     as Time
import qualified Database.PostgreSQL.Simple       as PGS
import qualified Database.PostgreSQL.Simple.Range as R
import           GHC.Int                          (Int64)
import           Opaleye                          (Field, FieldNullable, Select,
                                                   SelectArr, (.==), (.>))
import qualified Opaleye                          as O
import qualified Opaleye.Field                    as F
import qualified Opaleye.Internal.Aggregate       as IA
import qualified Opaleye.Internal.Column          as O (unColumn)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O (PrimExpr (..))
import qualified Opaleye.Internal.Operators       as O (relationValuedExpr)
import           Opaleye.Internal.RunQuery        (DefaultFromField)
import           Opaleye.Internal.MaybeFields     as OM
import           Opaleye.Internal.Locking         as OL
import qualified Connection
import qualified QuickCheck
import           System.Environment               (lookupEnv)
import           Test.Hspec
import qualified TypeFamilies                     ()

import Opaleye.Manipulation (Delete (Delete))

{-

Status
======

The Hspec tests are very superficial and pretty much the bare minimum
that needs to be tested.  The property tests are very thorough, but
could be made even more thorough.


Future
======

The property testing strategy is to define a denotation for SelectArrs
and to show that the denotation of two SelectArrs combined with an
operation is the same as using the operation to combine the
denotations.  The denotation that we will choose is roughly `Kleisli
[]` but we have to do IO operations over a Postgres connection so it's
slightly different in practice in a way that doesn't impinge on what I
am about to say.

For example, using brackets "[.]" to stand for denotation, we want to
ensure the property

* [f <<< g] = [f] <<< [g]

That is, running `f <<< g` on some input should be the same as running
`g` on the input, followed by running `f` on the output of `g`.
Likewise we want to ensure typeclass-general properties like

* [id] = id

* [f <*> g] = [f] <*> [g]

as well as Postgres-specific properties like

* [restrict] = guard

* [limit n q] = arr (take n) . [q]

The property tests are not written quite as neatly as this because
there is a lot of scaffolding to make things line up.  It's probably
possible to simplify the property tests though.

-}

required :: String -> O.TableFields (O.Field a) (O.Field a)
required = O.requiredTableField

twoIntTable :: String
            -> O.Table (Field O.SqlInt4, Field O.SqlInt4)
                       (Field O.SqlInt4, Field O.SqlInt4)
twoIntTable n = O.table n (PP.p2 (required "column1", required "column2"))

table1 :: O.Table (Field O.SqlInt4, Field O.SqlInt4)
                  (Field O.SqlInt4, Field O.SqlInt4)
table1 = twoIntTable "table1"

table1F :: O.Table (Field O.SqlInt4, Field O.SqlInt4)
                   (Field O.SqlInt4, Field O.SqlInt4)
table1F = fmap (\(col1, col2) -> (col1 + col2, col1 - col2)) table1

-- This is implicitly testing our ability to handle upper case letters
-- in table names.
table2 :: O.Table (Field O.SqlInt4, Field O.SqlInt4)
                  (Field O.SqlInt4, Field O.SqlInt4)
table2 = twoIntTable "TABLE2"

table3 :: O.Table (Field O.SqlInt4, Field O.SqlInt4)
                  (Field O.SqlInt4, Field O.SqlInt4)
table3 = twoIntTable "table3"

table4 :: O.Table (Field O.SqlInt4, Field O.SqlInt4)
                  (Field O.SqlInt4, Field O.SqlInt4)
table4 = twoIntTable "table4"

table5 :: O.Table (Maybe (Field O.SqlInt4), Maybe (Field  O.SqlInt4))
                  (Field O.SqlInt4, Field O.SqlInt4)
table5 = O.tableWithSchema "public" "table5"
  (PP.p2 (O.optionalTableField "column1", O.optionalTableField "column2"))

table6 :: O.Table (Field O.SqlText, Field O.SqlText)
                  (Field O.SqlText, Field O.SqlText)
table6 = O.table "table6" (PP.p2 (required "column1", required "column2"))

table7 :: O.Table (Field O.SqlText, Field O.SqlText)
                  (Field O.SqlText, Field O.SqlText)
table7 = O.table "table7" (PP.p2 (required "column1", required "column2"))

table8 :: O.Table (Field O.SqlJson) (Field O.SqlJson)
table8 = O.table "table8" (required "column1")

table9 :: O.Table (Field O.SqlJsonb) (Field O.SqlJsonb)
table9 = O.table "table9" (required "column1")

table10 :: O.Table (Field O.SqlInt4) (Field O.SqlInt4)
table10 = O.table "table10" (required "column1")

tableKeywordColNames :: O.Table (Field O.SqlInt4, Field O.SqlInt4)
                                (Field O.SqlInt4, Field O.SqlInt4)
tableKeywordColNames = O.table "keywordtable"
  (PP.p2 (required "column", required "where"))

table1Q :: Select (Field O.SqlInt4, Field O.SqlInt4)
table1Q = O.selectTable table1

table2Q :: Select (Field O.SqlInt4, Field O.SqlInt4)
table2Q = O.selectTable table2

table3Q :: Select (Field O.SqlInt4, Field O.SqlInt4)
table3Q = O.selectTable table3

table6Q :: Select (Field O.SqlText, Field O.SqlText)
table6Q = O.selectTable table6

table7Q :: Select (Field O.SqlText, Field O.SqlText)
table7Q = O.selectTable table7

table8Q :: Select (Field O.SqlJson)
table8Q = O.selectTable table8

table9Q :: Select (Field O.SqlJsonb)
table9Q = O.selectTable table9

table1dataG :: Num a => [(a, a)]
table1dataG = [ (1, 100)
              , (1, 100)
              , (1, 200)
              , (2, 300) ]

table1data :: [(Int, Int)]
table1data = table1dataG

table1fielddata :: [(Field O.SqlInt4, Field O.SqlInt4)]
table1fielddata = table1dataG

table2dataG :: Num a => [(a, a)]
table2dataG = [ (1, 100)
              , (3, 400) ]

table2data :: [(Int, Int)]
table2data = table2dataG

table2fielddata :: [(Field O.SqlInt4, Field O.SqlInt4)]
table2fielddata = table2dataG

table3dataG :: Num a => [(a, a)]
table3dataG = [ (1, 50) ]

table3data :: [(Int, Int)]
table3data = table3dataG

table3fielddata :: [(Field O.SqlInt4, Field O.SqlInt4)]
table3fielddata = table3dataG

table4dataG :: Num a => [(a, a)]
table4dataG = [ (1, 10)
              , (2, 20) ]

table4data :: [(Int, Int)]
table4data = table4dataG

table4fielddata :: [(Field O.SqlInt4, Field O.SqlInt4)]
table4fielddata = table4dataG

table6data :: [(String, String)]
table6data = [("xy", "a"), ("z", "a"), ("more text", "a")]

table6fielddata :: [(Field O.SqlText, Field O.SqlText)]
table6fielddata = map (\(field1, field2) ->
  (O.sqlString field1, O.sqlString field2)) table6data

table7data :: [(String, String)]
table7data = [("foo", "c"), ("bar", "a"), ("baz", "b")]

table7fielddata :: [(Field O.SqlText, Field O.SqlText)]
table7fielddata = map (O.sqlString *** O.sqlString) table7data

table8data :: [Json.Value]
table8data = [ Json.object
               [ "a" Json..= ([10,20..100] :: [Int])
               , "b" Json..= Json.object ["x" Json..= (42 :: Int)]
               , "c" Json..= (21 :: Int)
               ]
             ]

table8fielddata :: [Field O.SqlJson]
table8fielddata = map O.sqlValueJSON table8data

table9fielddata :: [Field O.SqlJsonb]
table9fielddata = map O.sqlValueJSONB table8data

-- We have to quote the table names here because upper case letters in
-- table names are treated as lower case unless the name is quoted!
dropAndCreateTable :: String -> (String, [String]) -> PGS.Query
dropAndCreateTable fieldType (t, cols) = String.fromString drop_
  where drop_ = "DROP TABLE IF EXISTS \"public\".\"" ++ t ++ "\";"
                ++ "CREATE TABLE \"public\".\"" ++ t ++ "\""
                ++ " (" ++ commas cols ++ ");"
        integer c = "\"" ++ c ++ "\"" ++ " " ++ fieldType
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

dropAndCreateTablePk :: (String, [String]) -> PGS.Query
dropAndCreateTablePk (t, cols) = String.fromString drop_
  where drop_ = "DROP TABLE IF EXISTS \"public\".\"" ++ t ++ "\";"
                ++ "CREATE TABLE \"public\".\"" ++ t ++ "\""
                ++ " (" ++ allFields ++ ");"
        pk c = "\"" ++ c ++ "\"" ++ " integer primary key"
        integer c = "\"" ++ c ++ "\"" ++ " integer"
        commas = L.intercalate ","
        allFields = commas $ [pk $ head cols] ++ map integer (tail cols)

type Table_ = (String, [String])

-- This should ideally be derived from the table definition above
fields2 :: String -> Table_
fields2 t = (t, ["column1", "column2"])

-- This should ideally be derived from the table definition above
tables :: [Table_]
tables = map fields2 ["table1", "TABLE2", "table3", "table4"]
         ++ [("keywordtable", ["column", "where"])]

serialTables :: [Table_]
serialTables = map fields2 ["table5"]

textTables :: [Table_]
textTables = map fields2 ["table6", "table7"]

jsonTables :: [Table_]
jsonTables = [("table8", ["column1"])]

jsonbTables :: [Table_]
jsonbTables = [("table9", ["column1"])]

conflictTables :: [Table_]
conflictTables = [("table10", ["column1"])]

dropAndCreateDB :: PGS.Connection -> IO ()
dropAndCreateDB conn = do
  mapM_ execute tables
  mapM_ executeTextTable textTables
  mapM_ executeSerial serialTables
  mapM_ executeJson jsonTables
  mapM_ executeConflict conflictTables
  mapM_ executeJsonb jsonbTables
  where execute = PGS.execute_ conn . dropAndCreateTableInt
        executeTextTable = PGS.execute_ conn . dropAndCreateTableText
        executeSerial = PGS.execute_ conn . dropAndCreateTableSerial
        executeJson = PGS.execute_ conn . dropAndCreateTableJson
        executeConflict = PGS.execute_ conn . dropAndCreateTablePk
        executeJsonb = PGS.execute_ conn . dropAndCreateTableJsonb

type Test = SpecWith PGS.Connection

testH :: D.Default O.FromFields fields haskells =>
         Select fields
         -> ([haskells] -> IO expectation)
         -> PGS.Connection
         -> IO expectation

testH q p conn = do
  result <- O.runSelect conn q
  p result

selectShouldReturnSorted :: (D.Default O.FromFields fields haskells
                            , Show haskells, Ord haskells) =>
         Select fields
         -> [haskells]
         -> PGS.Connection
         -> Expectation
selectShouldReturnSorted q expected = testH q (\res ->
  L.sort res `shouldBe` L.sort expected)

testSelect :: Test
testSelect = it "selects" $ table1Q `selectShouldReturnSorted` table1data

testProduct :: Test
testProduct = it "joins tables" $
    select `selectShouldReturnSorted` A.liftA2 (,) table1data table2data
  where select = table1Q &&& table2Q

testRestrict :: Test
testRestrict = it "restricts the rows returned" $
    select `selectShouldReturnSorted` filter ((== 1) . fst) (L.sort table1data)
  where select = proc () -> do
          t <- table1Q -< ()
          O.restrict -< fst t .== 1
          Arr.returnA -< t

testRestrictExists :: Test
testRestrictExists = it "restricts the rows returned with EXISTS" $
    select `selectShouldReturnSorted` filter ((== 1) . fst) (L.sort table1data)
  where select = proc () -> do
          t <- table1Q -< ()
          () <- O.restrictExists (proc t -> do
                            t' <- table1Q -< ()
                            O.restrict -< fst t' .> fst t) -< t
          Arr.returnA -< t

testRestrictNotExists :: Test
testRestrictNotExists = it "restricts the rows returned with NOT EXISTS" $
    select `selectShouldReturnSorted` filter ((== 2) . fst)  (L.sort table1data)
  where select = proc () -> do
          t <- table1Q -< ()
          () <- O.restrictNotExists (proc t -> do
                               t' <- table1Q -< ()
                               O.restrict -< fst t' .> fst t) -< t
          Arr.returnA -< t

testIn :: Test
testIn = it "restricts values to a range" $
    select `selectShouldReturnSorted` filter (flip elem [100, 200] . snd)
                                             (L.sort table1data)
  where select = proc () -> do
          t <- table1Q -< ()
          O.restrict -< O.in_ [O.sqlInt4 100, O.sqlInt4 200] (snd t)
          O.restrict -< O.not (O.in_ [] (fst t)) -- Making sure empty lists work.
          Arr.returnA -< t

testNum :: Test
testNum = it "" $ select `selectShouldReturnSorted` map op table1data
  where select :: Select (Field O.SqlInt4)
        select = proc () -> do
          t <- table1Q -< ()
          Arr.returnA -< op t
        op :: Num a => (a, a) -> a
        op (x, y) = abs (x - 5) * signum (x - 4) * (y * y + 1)

testDiv :: Test
testDiv = it "" $ select `selectShouldReturnSorted` map (op . toDoubles) table1data
  where select :: Select (Field O.SqlFloat8)
        select = proc () -> do
          t <- Arr.arr (doubleOfInt *** doubleOfInt) <<< table1Q -< ()
          Arr.returnA -< op t
        op :: Fractional a => (a, a) -> a
        -- Choosing 0.5 here as it should be exactly representable in
        -- floating point
        op (x, y) = y / x * 0.5
        toDoubles :: (Int, Int) -> (Double, Double)
        toDoubles = fromIntegral *** fromIntegral

        doubleOfInt = O.unsafeCast "float8"

-- TODO: need to implement and test case_ returning tuples
testCase :: Test
testCase = it "" $ q `selectShouldReturnSorted` expected
  where q :: Select (Field O.SqlInt4)
        q = table1Q >>> proc (i, j) -> do
          Arr.returnA -< O.case_ [(j .== 100, 12), (i .== 1, 21)] 33
        expected :: [Int]
        expected = [12, 12, 21, 33]

-- This tests case_ with an empty list of cases, to make sure it generates valid
-- SQL.
testCaseEmpty :: Test
testCaseEmpty = it "" $ q `selectShouldReturnSorted` expected
  where q :: Select (Field O.SqlInt4)
        q = table1Q >>> proc _ ->
          Arr.returnA -< O.case_ [] 33
        expected :: [Int]
        expected = [33, 33, 33, 33]

testDistinct :: Test
testDistinct =
  it "" $ O.distinct table1Q `selectShouldReturnSorted` L.nub table1data


testDistinctOn :: Test
testDistinctOn = do
    let distinctOn p q = \conn -> do
          let expected = L.nubBy (F.on (==) p) $ L.sortOn p table1data
          testH q (\r -> L.sort r `shouldBe` L.sort expected) conn

        distinctOnBy proj ord q = \conn -> do
          let expected = L.nubBy ((==) `F.on` proj) $ L.sortOn (proj &&& ord) triples
          testH q (\r -> L.sort r `shouldBe` L.sort expected) conn

    it "distinct on ()" $
        let p = const ()
            q = O.distinctOn p table1Q
        in distinctOn p q
    it "distinct on (col1)" $
        let p = fst
            q = O.distinctOn p table1Q
        in distinctOn p q
    it "distinct on (col1, col2)" $
        let p = fst &&& snd
            q = O.distinctOn p table1Q
        in distinctOn p q

    let f1 (x,_,_) = x
        f2 (_,y,_) = y
        f3 (_,_,z) = z

    it "distinct on () order by col1" $
        let proj = const ()
            ord  = f1
            q = O.distinctOnBy proj (O.asc ord) $ O.values pgTriples
        in distinctOnBy proj ord q
    it "distinct on (col1) order by col2" $
        let proj = f1
            ord  = f2
            q = O.distinctOnBy proj (O.asc ord) $ O.values pgTriples
        in distinctOnBy proj ord q
    it "distinct on (col1, col2) order by col3" $
        let proj = f1 &&& f2
            ord  = f3
            q = O.distinctOnBy proj (O.asc ord) $ O.values pgTriples
        in distinctOnBy proj ord q
    it "distinct on (col3) order by col2 desc" $ \conn -> do
        let proj = f3
            ord  = f2
            q = O.distinctOnBy proj (O.desc ord) $ O.values pgTriples
            expected = L.nubBy ((==) `F.on` proj) . L.reverse $
                L.sortOn (proj &&& ord) triples
        testH q (\r -> L.sort r `shouldBe` L.sort expected) conn
    where

        pgTriples :: [(O.Field O.SqlInt8, O.Field O.SqlInt8, O.Field O.SqlText)]
        pgTriples = (\(x,y,z) ->
                      (O.sqlInt8 x, O.sqlInt8 y, O.sqlStrictText z)) <$> triples

        triples :: [(Int64, Int64, T.Text)]
        triples =
            [ (1, 900, "a")
            , (1, 800, "a")
            , (2, 400, "a")
            , (2, 500, "b")
            , (2, 500, "b")
            , (4, 400, "b")
            , (4, 600, "b")
            , (4, 100, "b")
            ]

testAggregate :: Test
testAggregate = it "" $ O.aggregate (PP.p2 (O.groupBy, O.sumInt4))
                                           table1Q
                      `selectShouldReturnSorted` [ (1, 400) :: (Int, Int64)
                                                 , (2, 300) ]

testAggregate0 :: Test
testAggregate0 = it "" $    O.aggregate (PP.p2 (O.sum, O.sumInt4))
                                        (proc () -> do
                                            r <- table1Q -< ()
                                            O.restrict -< O.sqlBool False
                                            Arr.returnA -< r)
                         `selectShouldReturnSorted` ([] :: [(Int, Int64)])

testAggregateFunction :: Test
testAggregateFunction = it "" $
                            O.aggregate (PP.p2 (O.groupBy, O.sumInt4))
                                        (fmap (\(x, y) -> (x + 1, y)) table1Q)
                      `selectShouldReturnSorted` [ (2, 400) :: (Int, Int64)
                                                 , (3, 300) ]

testAggregateProfunctor :: Test
testAggregateProfunctor = it "" $
    q `selectShouldReturnSorted` [ (1, 1200) :: (Int, Int64), (2, 300)]
  where q = O.aggregate (PP.p2 (O.groupBy, countsum)) table1Q
        countsum = P.dimap (\x -> (x,x))
                           (\(x, y) -> x * y)
                           (PP.p2 (O.sumInt4, O.count))

testStringArrayAggregate :: Test
testStringArrayAggregate = it "" $
    q `selectShouldReturnSorted` [(map fst table6data,
                                   minimum (map snd table6data))]
  where q = O.aggregate (PP.p2 (O.arrayAgg, O.min)) table6Q

testValueJsonAggregate :: Test
testValueJsonAggregate =
  it "" $
    testH
      q
      ( \((res : _) :: [Json.Value]) ->
          Just res `shouldBe` r
      )
  where
    r = Json.decode "[{\"summary\": \"xy\", \"details\": \"a\"}, {\"summary\": \"z\", \"details\": \"a\"}, {\"summary\": \"more text\", \"details\": \"a\"}]"
    q = O.aggregate O.jsonAgg $ do
      (firstCol, secondCol) <- O.selectTable table6
      return
        . O.jsonBuildObject
        $ O.jsonBuildObjectField "summary" firstCol
          <> O.jsonBuildObjectField "details" secondCol

testByteStringJsonAggregate :: Test
testByteStringJsonAggregate =
  it "" $
    testH
      q
      ( \((res : _) :: [SBS.ByteString]) ->
          Just res `shouldBe` r
      )
  where
    r :: Maybe SBS.ByteString = Just "[{\"summary\" : \"xy\", \"details\" : \"a\"}, {\"summary\" : \"z\", \"details\" : \"a\"}, {\"summary\" : \"more text\", \"details\" : \"a\"}]"
    q = O.aggregate O.jsonAgg $ do
      (firstCol, secondCol) <- O.selectTable table6
      return
        . O.jsonBuildObject
        $ O.jsonBuildObjectField "summary" firstCol
          <> O.jsonBuildObjectField "details" secondCol

testStringJsonAggregateWithJoin :: Test
testStringJsonAggregateWithJoin =
  it "" $
    testH
      q
      ( \((res : _) :: [Json.Value]) ->
          Just res `shouldBe` r
      )
  where
    r = Json.decode "[{\"id\" : 1, \"name\" : 100, \"blog_post\" : {\"summary\" : 1, \"details\" : 100}}, {\"id\" : 1, \"name\" : 100, \"blog_post\" : {\"summary\" : 1, \"details\" : 100}}, {\"id\" : 1, \"name\" : 200, \"blog_post\" : {\"summary\" : 1, \"details\" : 100}}]"
    q = O.aggregate O.jsonAgg $ do
      (firstCol, secondCol) <- O.selectTable table1
      (firstCol2, secondCol2) <- O.selectTable table2
      O.viaLateral O.restrict (firstCol .== firstCol2)
      let blog_post =
            O.jsonBuildObject $
              O.jsonBuildObjectField "summary" firstCol2
                <> O.jsonBuildObjectField "details" secondCol2
      return
        . O.jsonBuildObject
        $ O.jsonBuildObjectField "id" firstCol
          <> O.jsonBuildObjectField "name" secondCol
          <> O.jsonBuildObjectField "blog_post" blog_post

testStringAggregate :: Test
testStringAggregate = it "" $ q `selectShouldReturnSorted` expected
  where q = O.aggregate (PP.p2 ((O.stringAgg . O.sqlString) "_", O.groupBy))
                        table6Q
        expected = [(
          (foldl1 (\x y -> x ++ "_" ++ y) . map fst) table6data ,
          head (map snd table6data))]

-- | Using aggregateOrdered applies the ordering to all aggregates.

testStringArrayAggregateOrdered :: Test
testStringArrayAggregateOrdered = it "" $ q `selectShouldReturnSorted` expected
  where q = O.aggregateOrdered (O.asc snd)
                               (PP.p2 (O.arrayAgg, O.stringAgg . O.sqlString $ ",")) table7Q
        expected = [( map fst sortedData
                      , L.intercalate "," . map snd $ sortedData
                      )
                     ]
        sortedData = L.sortBy (Ord.comparing snd) table7data

-- | Using orderAggregate you can apply different orderings to
-- different aggregates.

testMultipleAggregateOrdered :: Test
testMultipleAggregateOrdered = it "" $ q `selectShouldReturnSorted` expected
  where q = O.aggregate ((,) <$> IA.orderAggregate (O.asc snd)
                                                   (P.lmap fst O.arrayAgg)
                             <*> IA.orderAggregate (O.desc snd)
                                                   (P.lmap snd (O.stringAgg . O.sqlString $ ","))
                        ) table7Q
        expected = [( map fst . L.sortBy (Ord.comparing snd) $ table7data
                      , L.intercalate ","
                        . map snd
                        . L.sortBy (Ord.comparing (Ord.Down . snd)) $ table7data
                      )
                     ]

-- | Applying an order to an ordered aggregate overwrites the old
-- order, just like with ordered queries.
--
testOverwriteAggregateOrdered :: Test
testOverwriteAggregateOrdered = it "" $ q `selectShouldReturnSorted` expected
  where q = O.aggregate ( IA.orderAggregate (O.asc snd)
                        . IA.orderAggregate (O.desc snd)
                        $ PP.p2 (O.arrayAgg, O.max)
                        ) table7Q
        expected = [( map fst (L.sortBy (Ord.comparing snd) table7data)
                      , maximum (map snd table7data)
                      )
                     ]

testCountRows0 :: Test
testCountRows0 = it "" $ q `selectShouldReturnSorted` [0 :: Int64]
  where q        = O.countRows (proc () -> do
                                   r <- table7Q -< ()
                                   O.restrict -< O.sqlBool False
                                   Arr.returnA -< r)

testCountRows3 :: Test
testCountRows3 = it "" $ q `selectShouldReturnSorted` [3 :: Int64]
  where q        = O.countRows table7Q

selectShouldReturnSortBy :: O.Order (Field O.SqlInt4, Field O.SqlInt4)
                -> ((Int, Int) -> (Int, Int) -> Ordering)
                -> (PGS.Connection -> Expectation)
selectShouldReturnSortBy orderQ order = testH (O.orderBy orderQ table1Q)
                                  (L.sortBy order table1data `shouldBe`)

testOrderBy :: Test
testOrderBy = it "" $ selectShouldReturnSortBy (O.desc snd)
                           (flip (Ord.comparing snd))

testOrderBy2 :: Test
testOrderBy2 = it "" $ selectShouldReturnSortBy (O.desc fst <> O.asc snd)
                            (flip (Ord.comparing fst) <> Ord.comparing snd)

testOrderBySame :: Test
testOrderBySame = it "" $ selectShouldReturnSortBy (O.desc fst <> O.asc fst)
                               (flip (Ord.comparing fst) <> Ord.comparing fst)

testOrderExact :: Test
testOrderExact = it "" $ testH (O.orderBy (O.exact cols snd) table1Q)
                               (result `shouldBe`)
  where cols   = map O.toFields [300,200::Int]
        result = [ (2::Int, 300::Int)
                 , (1, 200)
                 , (1, 100)
                 , (1, 100)
                 ]

limitOrderShouldMatch :: (Select (Field O.SqlInt4, Field O.SqlInt4)
                          -> Select (Field O.SqlInt4, Field O.SqlInt4))
                      -> ([(Int, Int)] -> [(Int, Int)])
                      -> (PGS.Connection -> Expectation)
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
testDistinctAndAggregate = it "" $ q `selectShouldReturnSorted` expectedResult
  where q = O.distinct table1Q
            &&& O.aggregate (PP.p2 (O.groupBy, O.sumInt4)) table1Q
        expectedResult = A.liftA2 (,) (L.nub table1data)
                                      [(1 :: Int, 400 :: Int64), (2, 300)]

one :: Select (Field O.SqlInt4)
one = Arr.arr (const (1 :: Field O.SqlInt4))

-- The point of the "double" tests is to ensure that we do not
-- introduce name clashes in the operations which create new field names
testDoubleH :: (Show haskells, Eq haskells, D.Default O.FromFields fields haskells)
            => (SelectArr () (Field O.SqlInt4) -> SelectArr () fields) -> [haskells]
            -> (PGS.Connection -> Expectation)
testDoubleH q expected1 = testH (q one &&& q one) (`shouldBe` expected2)
  where expected2 = A.liftA2 (,) expected1 expected1

testDoubleDistinct :: Test
testDoubleDistinct = it "" $ testDoubleH O.distinct [1 :: Int]

testDoubleAggregate :: Test
testDoubleAggregate = it "" $ testDoubleH (O.aggregate O.count) [1 :: Int64]

testDoubleLeftJoin :: Test
testDoubleLeftJoin = it "" $ testDoubleH lj [(1 :: Int, Just (1 :: Int))]
  where lj :: Select (Field O.SqlInt4)
          -> Select (Field O.SqlInt4, FieldNullable O.SqlInt4)
        lj q = O.leftJoin q q (uncurry (.==))

testDoubleValues :: Test
testDoubleValues = it "" $ testDoubleH v [1 :: Int]
  where v :: Select (Field O.SqlInt4) -> Select (Field O.SqlInt4)
        v _ = O.values [1]

testDoubleUnionAll :: Test
testDoubleUnionAll = it "" $ testDoubleH u [1 :: Int, 1]
  where u q = q `O.unionAll` q

aLeftJoin :: Select ((Field O.SqlInt4, Field O.SqlInt4),
                    (FieldNullable O.SqlInt4, FieldNullable O.SqlInt4))
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
  where q :: Select ((Field O.SqlInt4, Field O.SqlInt4),
                    ((FieldNullable O.SqlInt4, FieldNullable O.SqlInt4),
                     (FieldNullable O.SqlInt4,
                      FieldNullable O.SqlInt4)))
        q = O.leftJoin table3Q aLeftJoin cond

        cond (x, y) = fst x .== fst (fst y)

        expected :: [((Int, Int), ((Maybe Int, Maybe Int), (Maybe Int, Maybe Int)))]
        expected = [ ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 200), (Just 1, Just 50))) ]

testThreeWayProduct :: Test
testThreeWayProduct = it "" $ testH q (`shouldBe` expected)
  where q = A.liftA3 (,,) table1Q table2Q table3Q
        expected = A.liftA3 (,,) table1data table2data table3data

testValues :: Test
testValues = it "" $ testH (O.values values) (values' `shouldBe`)
  where values :: [(Field O.SqlInt4, Field O.SqlInt4)]
        values = [ (1, 10)
                 , (2, 100) ]
        values' :: [(Int, Int)]
        values' = [ (1, 10)
                  , (2, 100) ]

{- FIXME: does not yet work
testValuesDouble :: Test
testValuesDouble = testG (O.values values) (values' ==)
  where values :: [(Field O.SqlInt4, Field O.SqlFloat8)]
        values = [ (1, 10.0)
                 , (2, 100.0) ]
        values' :: [(Int, Double)]
        values' = [ (1, 10.0)
                  , (2, 100.0) ]
-}

testValuesEmpty :: Test
testValuesEmpty = it "" $ testH (O.values values) (values' `shouldBe`)
  where values :: [Field O.SqlInt4]
        values = []
        values' :: [Int]
        values' = []

testUnionAll :: Test
testUnionAll = it "" $ (table1Q `O.unionAll` table2Q)
                       `selectShouldReturnSorted` (table1data ++ table2data)

testTableFunctor :: Test
testTableFunctor = it "" $ testH (O.selectTable table1F) (result `shouldBe`)
  where result = fmap (\(col1, col2) -> (col1 + col2, col1 - col2)) table1data

recursive = O.withRecursive table1Q $ \(n, x) -> do
          O.where_ (n O..< 5)
          pure (n + 1, x + 1)

testWithRecursive :: Test
testWithRecursive = it "with recursive" $ testH recursive (`shouldBe` expected)
  where expected = withRecursive [] table1data $ \(n, x) -> do
          guard (n < 5)
          pure (n + 1, x + 1)
        withRecursive r s f =
          let r' = s ++ (r >>= f)
          in if r' == r then r else withRecursive r' s f

testWith :: Test
testWith = it "with" $ testH with (`shouldBe` expected)
  where with = O.with table1Q $ \t -> (,) <$> t <*> table2Q
        expected = (,) <$> table1data <*> table2data

-- TODO: This is getting too complicated
testUpdate :: Test
testUpdate = it "" $ \conn -> do
  _ <- O.runUpdate_ conn O.Update { O.uTable = table4
                                  , O.uUpdateWith = update
                                  , O.uWhere = cond
                                  , O.uReturning = O.rCount }
  result <- runSelectTable4 conn
  result `shouldBe` expected

  _ <- O.runDelete_ conn O.Delete { O.dTable = table4
                                  , O.dWhere = condD
                                  , O.dReturning = O.rCount }
  resultD <- runSelectTable4 conn
  resultD `shouldBe` expectedD

  returned <- O.runInsert_ conn O.Insert { O.iTable = table4
                                         , O.iRows = insertT
                                         , O.iReturning = O.rReturning returning
                                         , O.iOnConflict = Nothing }
  _ <- O.runInsert_ conn O.Insert { O.iTable = table4
                                  , O.iRows = insertTMany
                                  , O.iReturning = O.rCount
                                  , O.iOnConflict = Nothing }
  resultI <- runSelectTable4 conn

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
        runSelectTable4 conn = O.runSelect conn (O.selectTable table4)

        insertT :: [(Field O.SqlInt4, Field O.SqlInt4)]
        insertT = [(1, 2), (3, 5)]

        insertTMany :: [(Field O.SqlInt4, Field O.SqlInt4)]
        insertTMany = [(20, 30), (40, 50)]

        expectedI :: [(Int, Int)]
        expectedI = [(1, 10), (1, 2), (3, 5), (20, 30), (40, 50)]
        returning (x, y) = x - y
        expectedR :: [Int]
        expectedR = [-1, -2]

testDeleteReturning :: Test
testDeleteReturning = it "" $ \conn -> do
  result <- O.runDelete_ conn delete
  _ <- O.runInsert_ conn O.Insert { O.iTable = table4
                                  , O.iRows = [(40,50)]
                                    :: [(Field O.SqlInt4, Field O.SqlInt4)]
                                  , O.iReturning = O.rCount
                                  , O.iOnConflict = Nothing } :: IO Int64
  result `shouldBe` expected
  where delete = Delete table cond returning
        table = table4
        cond (_, y) = y .> 45
        returning = O.rReturning id
        expected = [(40, 50)] :: [(Int, Int)]

testInsertConflict :: Test
testInsertConflict = it "inserts with conflicts" $ \conn -> do
  _ <- O.runDelete_ conn O.Delete { O.dTable = table10
                                  , O.dWhere = const $ O.toFields True
                                  , O.dReturning = O.rCount }
  returned <- O.runInsert_ conn O.Insert { O.iTable = table10
                                         , O.iRows = insertT
                                         , O.iReturning = O.rReturning id
                                         , O.iOnConflict = Nothing }
  extras <- O.runInsert_ conn O.Insert { O.iTable = table10
                                       , O.iRows = conflictsT
                                       , O.iReturning = O.rReturning id
                                       , O.iOnConflict = Just O.doNothing }
  moreExtras <- O.runInsert_ conn O.Insert { O.iTable = table10
                                           , O.iRows = moreConflictsT
                                           , O.iReturning = O.rCount
                                           , O.iOnConflict = Just O.doNothing }

  returned `shouldBe` afterInsert
  extras `shouldBe` afterConflicts
  moreExtras `shouldBe` 1
  runSelectTable10 conn `shouldReturn` allRows

  O.runInsert_ conn O.Insert { O.iTable = table10
                             , O.iRows = insertT
                             , O.iReturning = O.rCount
                             , O.iOnConflict = Nothing }
    `shouldThrow` (\ (_ :: PGS.SqlError) -> True)

  where insertT :: [Field O.SqlInt4]
        insertT = [1, 2]

        conflictsT :: [Field O.SqlInt4]
        conflictsT = [1, 3]

        moreConflictsT :: [Field O.SqlInt4]
        moreConflictsT = [3, 4]

        afterInsert :: [Int]
        afterInsert = [1, 2]

        afterConflicts :: [Int]
        afterConflicts = [3]

        allRows :: [Int]
        allRows = [1, 2, 3, 4]

        runSelectTable10 conn = O.runSelect conn (O.selectTable table10)

testKeywordColNames :: Test
testKeywordColNames = it "" $ \conn -> do
  let q :: IO [(Int, Int)]
      q = O.runSelect conn (O.selectTable tableKeywordColNames)
  _ <- q
  True `shouldBe` True

testInsertSerial :: Test
testInsertSerial = it "" $ \conn -> do
  _ <- runInsert conn table5 (Just 10, Just 20)
  _ <- runInsert conn table5 (Just 30, Nothing)
  _ <- runInsert conn table5 (Nothing, Nothing)
  _ <- runInsert conn table5 (Nothing, Just 40)

  resultI <- O.runSelect conn (O.selectTable table5)

  resultI `shouldBe` expected

  where expected :: [(Int, Int)]
        expected = [ (10, 20)
                   , (30, 1)
                   , (1, 2)
                   , (2, 40) ]
        runInsert conn table row =
          O.runInsert_ conn O.Insert { O.iTable = table
                                     , O.iRows = [row]
                                     , O.iReturning = O.rCount
                                     , O.iOnConflict = Nothing }

testInSelect :: Test
testInSelect = it "" $ \conn -> do
  let q (x, e) = testH (O.inSelect x (O.selectTable table1)) (`shouldBe` [e]) conn

  mapM_ (q . (\x ->      (x,        True)))  table1dataG
  mapM_ (q . (\(x, y) -> ((x, y+1), False))) table1dataG

  -- and r && and s `shouldBe` True

testAtTimeZone :: Test
testAtTimeZone =
  it "" $ testH (A.pure (O.timestamptzAtTimeZone t (O.sqlString "CET")))
                (`shouldBe` [t'])
  where t = O.sqlUTCTime (Time.UTCTime d (Time.secondsToDiffTime 3600))
        t' = Time.LocalTime d (Time.TimeOfDay 2 0 0)
        d = Time.fromGregorian 2015 1 1

testArrayLiterals :: Test
testArrayLiterals = it "" $ testH (A.pure $ O.sqlArray O.sqlInt4 vals)
                                  (`shouldBe` [vals])
  where vals = [1,2,3]

-- This test fails without the explicit cast in pgArray since postgres
-- can't determine the type of the array.

testEmptyArray :: Test
testEmptyArray = it "" $ testH (A.pure $ O.sqlArray O.sqlInt4 [])
                               (`shouldBe` [[] :: [Int]])

-- This test fails without the explicit cast in pgArray since postgres
-- defaults the numbers to 'integer' but postgresql-simple expects 'float8'.

testFloatArray :: Test
testFloatArray = it "" $ testH (A.pure $ O.sqlArray O.sqlDouble doubles)
                               (`shouldBe` [doubles])
  where
    doubles = [1 :: Double, 2]

testArrayIndex :: Test
testArrayIndex = it "correctly indexes an array" $
  testH (A.pure $ O.sqlArray O.sqlInt4 [5,6,7] `O.index` O.sqlInt4 3)
        (`shouldBe` ([Just 7] :: [Maybe Int]))

testArrayIndexOOB :: Test
testArrayIndexOOB = it "returns Nothing when the index is out of bounds" $
  testH (A.pure $ O.sqlArray O.sqlInt4 [5,6,7] `O.index` O.sqlInt4 8)
        (`shouldBe` ([Nothing] :: [Maybe Int]))

testSingletonArray :: Test
testSingletonArray = it "constructs a singleton PGInt8 array" $
  testH (A.pure $ O.singletonArray (O.sqlInt8 1))
        (`shouldBe` ([[1]] :: [[Int64]]))

testArrayAppend :: Test
testArrayAppend = it "appends two arrays" $
  testH (A.pure $ O.sqlArray O.sqlInt4 [5,6,7]
                  `O.arrayAppend` O.sqlArray O.sqlInt4 [1,2,3])
        (`shouldBe` ([[5,6,7,1,2,3]] :: [[Int]]))

testArrayPosition :: Test
testArrayPosition = do
  it "determines array position (SqlInt4)" $
    testH (A.pure (O.arrayPosition (O.sqlArray O.sqlInt4 [5,6,7]) 5))
          (`shouldBe` [Just (1 :: Int)])
  it "determines array position (NULL) (SqlInt4)" $
    testH (A.pure (O.arrayPosition (O.sqlArray O.sqlInt4 [5,6,7]) 999))
          (`shouldBe` [Nothing :: Maybe Int])
  it "determines array position (SqlInt8)" $
    testH (A.pure (O.arrayPosition (O.sqlArray O.sqlInt8 [5,6,7]) 5))
          (`shouldBe` [Just (1 :: Int)])
  it "determines array position (NULL) (SqlInt8)" $
    testH (A.pure (O.arrayPosition (O.sqlArray O.sqlInt8 [5,6,7]) 999))
          (`shouldBe` [Nothing :: Maybe Int])

testSqlElem :: Test
testSqlElem = do
  it "checks presence of the element (SqlInt4)" $
    testH (A.pure (O.sqlElem 5 (O.sqlArray O.sqlInt4 [5,6,7])))
          (`shouldBe` [True])
  it "checks absence of the element (SqlInt4)" $
    testH (A.pure (O.sqlElem 999 (O.sqlArray O.sqlInt4 [5,6,7])))
          (`shouldBe` [False])

type JsonTest a = SpecWith (Select (Field a) -> PGS.Connection -> Expectation)
-- Test opaleye's equivalent of c1->'c'
testJsonGetFieldValue :: (O.SqlIsJson a, DefaultFromField a Json.Value)
                      => Select (Field a) -> Test
testJsonGetFieldValue dataSelect = it "" $ testH q (`shouldBe` expected)
  where q = dataSelect >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.sqlStrictText "c"
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number 21]

-- Test opaleye's equivalent of c1->>'c'
testJsonGetFieldText :: (O.SqlIsJson a) => Select (Field a) -> Test
testJsonGetFieldText dataSelect = it "" $ testH q (`shouldBe` expected)
  where q = dataSelect >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..->> O.sqlStrictText "c"
        expected :: [Maybe T.Text]
        expected = [Just "21"]

-- Special Test for Github Issue #350 :
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/350
testRestrictWithJsonOp :: (O.SqlIsJson a) => Select (Field a) -> Test
testRestrictWithJsonOp dataSelect = it "restricts the rows returned by checking equality with a value extracted using JSON operator" $ testH select (`shouldBe` table8data)
  where select = dataSelect >>> proc col1 -> do
          t <- table8Q -< ()
          O.restrict -< nonNullOrFalse (O.toNullable col1 O..->> O.sqlStrictText "c") $ \x1 ->
                        nonNullOrFalse (O.toNullable (O.sqlStrictText "21")) $ \x2 ->
                        x1 .== x2
          Arr.returnA -< t

        nonNullOrFalse = flip (F.matchNullable (O.sqlBool False))

-- Test opaleye's equivalent of c1->'a'->2
testJsonGetArrayValue :: (O.SqlIsJson a, DefaultFromField a Json.Value)
                      => Select (Field a) -> Test
testJsonGetArrayValue dataSelect = it "" $ testH q (`shouldBe` expected)
  where q = dataSelect >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.sqlStrictText "a" O..-> O.sqlInt4 2
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number 30]

-- Test opaleye's equivalent of c1->'a'->>2
testJsonGetArrayText :: (O.SqlIsJson a) => Select (Field a) -> Test
testJsonGetArrayText dataSelect = it "" $ testH q (`shouldBe` expected)
  where q = dataSelect >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..-> O.sqlStrictText "a" O..->> O.sqlInt4 2
        expected :: [Maybe T.Text]
        expected = [Just "30"]

-- Test opaleye's equivalent of c1->>'missing'
-- Note that the missing field does not exist.
testJsonGetMissingField :: (O.SqlIsJson a) => Select (Field a) -> Test
testJsonGetMissingField dataSelect = it "" $ testH q (`shouldBe` expected)
  where q = dataSelect >>> proc c1 -> do
            Arr.returnA -< O.toNullable c1 O..->> O.sqlStrictText "missing"
        expected :: [Maybe T.Text]
        expected = [Nothing]

-- Test opaleye's equivalent of c1#>'{b,x}'
testJsonGetPathValue :: (O.SqlIsJson a, DefaultFromField a Json.Value)
                     => Select (Field a) -> Test
testJsonGetPathValue dataSelect = it "" $ testH q (`shouldBe` expected)
  where q = dataSelect >>> proc c1 -> do
              Arr.returnA -< O.toNullable c1 O..#> O.sqlArray O.sqlStrictText ["b", "x"]
        expected :: [Maybe Json.Value]
        expected = [Just $ Json.Number 42]

-- Test opaleye's equivalent of c1#>>'{b,x}'
testJsonGetPathText :: (O.SqlIsJson a) => Select (Field a) -> Test
testJsonGetPathText dataSelect = it "" $ testH q (`shouldBe` expected)
  where q = dataSelect >>> proc c1 -> do
              Arr.returnA -< O.toNullable c1 O..#>> O.sqlArray O.sqlStrictText ["b", "x"]
        expected :: [Maybe T.Text]
        expected = [Just "42"]

-- Test opaleye's equivalent of c1 @> '{"c":21}'::jsonb
testJsonbRightInLeft :: Test
testJsonbRightInLeft = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..@> O.sqlJSONB "{\"c\":21}"

-- Test opaleye's equivalent of '{"c":21}'::jsonb <@ c1
testJsonbLeftInRight :: Test
testJsonbLeftInRight = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< O.sqlJSONB "{\"c\":21}" O..<@ c1

-- Test opaleye's equivalent of c1 ? 'b'
testJsonbContains :: Test
testJsonbContains = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..? O.sqlStrictText "c"

-- Test opaleye's equivalent of c1 ? 'missing'
-- Note that the missing field does not exist.
testJsonbContainsMissing :: Test
testJsonbContainsMissing = it "" $ testH q (`shouldBe` [False])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..? O.sqlStrictText "missing"

-- Test opaleye's equivalent of c1 ?| array['b', 'missing']
testJsonbContainsAny :: Test
testJsonbContainsAny = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..?| O.sqlArray O.sqlStrictText ["b", "missing"]

-- Test opaleye's equivalent of c1 ?& array['a', 'b', 'c']
testJsonbContainsAll :: Test
testJsonbContainsAll = it "" $ testH q (`shouldBe` [True])
  where q = table9Q >>> proc c1 -> do
              Arr.returnA -< c1 O..?& O.sqlArray O.sqlStrictText ["a", "b", "c"]

testRangeOverlap :: Test
testRangeOverlap = it "generates overlap" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Field (O.SqlRange O.SqlInt4)
        range a b = O.sqlRange O.sqlInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure (range 3 7 `O.overlap` range 4 12)

testRangeDateOverlap :: Test
testRangeDateOverlap = it "generates time overlap" $ \conn -> do
    let date       = Time.fromGregorian 2015 1 1
        now        = Time.UTCTime date (Time.secondsToDiffTime 3600)
        later      = Time.addUTCTime 10 now
        range1     = O.sqlRange O.sqlUTCTime (R.Inclusive now) (R.Exclusive later)
        range2     = O.sqlRange O.sqlUTCTime R.NegInfinity R.PosInfinity
        rangeNow   = O.sqlRange O.sqlUTCTime (R.Inclusive now) (R.Inclusive now)
        qOverlap r = A.pure $ r `O.overlap` rangeNow
    testH (qOverlap range1) (`shouldBe` [True]) conn
    testH (qOverlap range2) (`shouldBe` [True]) conn
    testH (A.pure $ O.sqlUTCTime now   `O.liesWithin` range1) (`shouldBe` [True]) conn
    testH (A.pure $ O.sqlUTCTime later `O.liesWithin` range1) (`shouldBe` [False]) conn

testRangeLeftOf :: Test
testRangeLeftOf = it "generates 'left of'" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Field (O.SqlRange O.SqlInt4)
        range a b = O.sqlRange O.sqlInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure (range 1 10 O..<< range 100 110)

testRangeRightOf :: Test
testRangeRightOf = it "generates 'right of'" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Field (O.SqlRange O.SqlInt4)
        range a b = O.sqlRange O.sqlInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure (range 50 60 O..>> range 20 30)

testRangeRightExtension :: Test
testRangeRightExtension = it "generates right extension" $
    testH q (`shouldBe` [True])
  where range :: Int -> Int -> Field (O.SqlRange O.SqlInt4)
        range a b = O.sqlRange O.sqlInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure (range 1 20 O..&< range 18 20)

testRangeLeftExtension :: Test
testRangeLeftExtension = it "generates left extension" $
    testH q (`shouldBe` [True])
  where range :: Int -> Int -> Field (O.SqlRange O.SqlInt4)
        range a b = O.sqlRange O.sqlInt4 (R.Inclusive a) (R.Inclusive b)
        q = A.pure (range 7 20 O..&> range 5 10)

testRangeAdjacency :: Test
testRangeAdjacency = it "generates adjacency" $ testH q (`shouldBe` [True])
  where range :: Int -> Int -> Field (O.SqlRange O.SqlInt4)
        range a b = O.sqlRange O.sqlInt4 (R.Inclusive a) (R.Exclusive b)
        q = A.pure (range 1 2 O..-|- range 2 3)

testRangeBoundsEnum :: forall a b.
    ( Show a, Eq a, Enum a, O.IsRangeType b
    , DefaultFromField b a )
        => String -> (a -> Field b) -> a -> a -> Test
testRangeBoundsEnum msg mkCol x y = it msg $ \conn -> do
    -- bound functions for discrete range types return fields as from
    -- the form [x,y)
    let pgr = O.sqlRange mkCol
        ranges_expecteds =
          [ (pgr (R.Inclusive x) R.PosInfinity,   (Just x, Nothing))
          , (pgr R.NegInfinity   (R.Inclusive y), (Nothing, Just $ succ y))
          , (pgr (R.Exclusive x) (R.Exclusive y), (Just $ succ x, Just y))
          ]
        ranges    = map fst ranges_expecteds
        expecteds = map ((:[]) . snd) ranges_expecteds

    r <- mapM (O.runSelect conn . pure . (O.lowerBound &&& O.upperBound)) ranges
    r `shouldBe` expecteds

jsonTests :: (O.SqlIsJson a, DefaultFromField a Json.Value)
          => Select (Field a) -> Test
jsonTests t = do
  testJsonGetFieldValue   t
  testJsonGetFieldText    t
  testJsonGetMissingField t
  testJsonGetArrayValue   t
  testJsonGetArrayText    t
  testJsonGetPathValue    t
  testJsonGetPathText     t
  testRestrictWithJsonOp  t

testLiterals :: Test
testLiterals = do
  let testLiteral fn value = testH (pure (fn value)) (`shouldBe` [value])
      exampleDate = Time.fromGregorian 2018 11 29
      exampleTime = Time.TimeOfDay 11 22 33
      exampleUTCTime = Time.UTCTime exampleDate (Time.sinceMidnight exampleTime)
      exampleDatePadded = Time.fromGregorian 18 11 29
      exampleUTCTimePadded = Time.UTCTime exampleDatePadded (Time.sinceMidnight exampleTime)
  it "sqlString" $ testLiteral O.sqlString "Hello"
  it "sqlLazyByteString" $ testLiteral O.sqlLazyByteString "Hello"
  it "sqlNumeric" $ testLiteral O.sqlNumeric 3.14159
  it "sqlInt4" $ testLiteral O.sqlInt4 17
  it "sqlInt8" $ testLiteral O.sqlInt8 0x100000000
  it "sqlDouble" $ testLiteral O.sqlDouble 3.14
  it "sqlBool" $ testLiteral O.sqlBool True
  it "sqlUUID" $ testLiteral O.sqlUUID (read "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
  it "sqlDay" $ testLiteral O.sqlDay exampleDate
  it "sqlDayPadded" $ testLiteral O.sqlDay exampleDatePadded
  it "sqlUTCTime" $ testLiteral O.sqlUTCTime exampleUTCTime
  it "sqlUTCTimePadded" $ testLiteral O.sqlUTCTime exampleUTCTimePadded
  it "sqlLocalTime" $ testLiteral O.sqlLocalTime (Time.LocalTime exampleDate exampleTime)
  it "sqlLocalTimePadded" $ testLiteral O.sqlLocalTime (Time.LocalTime exampleDatePadded exampleTime)

  -- ZonedTime has no Eq instance, so we compare on the result of 'zonedTimeToUTC'
  it "sqlZonedTime" $
    let value = Time.utcToZonedTime Time.utc exampleUTCTime in
    testH (pure (O.sqlZonedTime value))
          (\r -> map Time.zonedTimeToUTC r `shouldBe` [Time.zonedTimeToUTC value])

  it "sqlZonedTimePadded" $
    let value = Time.utcToZonedTime Time.utc exampleUTCTimePadded in
    testH (pure (O.sqlZonedTime value))
          (\r -> map Time.zonedTimeToUTC r `shouldBe` [Time.zonedTimeToUTC value])

  it "sqlInterval" $ testLiteral O.sqlInterval (Time.calendarTimeTime 1)

-- Check that MaybeFields's "Nothings" are not distinct, even if we
-- fmap different values over their inner fields.
testMaybeFieldsDistinct :: Test
testMaybeFieldsDistinct = do
  it "MaybeFields distinct" $ testH query (`shouldBe` [Nothing :: Maybe Int])
  it "MaybeFields equality" $ testH query2 (`shouldBe` [True])
  where nothing_ = OM.nothingFields :: MaybeFields ()
        query :: Select (MaybeFields (Field O.SqlInt4))
        query = O.distinct (O.values [ fmap (const 0) nothing_
                                         , fmap (const 1) nothing_ ])
        query2 :: Select (Field O.SqlBool)
        query2 = pure ((fmap (const (0 :: Field O.SqlInt4)) nothing_)
                       O..=== fmap (const (1 :: Field O.SqlInt4)) nothing_)

testForUpdate :: Test
testForUpdate = do
  it "Returns same rows from a table" $
      testH (OL.forUpdate table1Q) (`shouldBe` table1data)

testAddIntervalFromDateToTimestamptz :: Test
testAddIntervalFromDateToTimestamptz = do
  it "date + interval = timestamptz" $ testH query (`shouldBe` [expectation])
  where query :: Select (Field O.SqlTimestamp)
        query = pure $ (O.toFields d :: Field O.SqlDate)
                       `O.addInterval`
                       (O.toFields c :: Field O.SqlInterval)

        d :: Time.Day
        d = Time.ModifiedJulianDay 0 -- = 1858-11-17

        -- 1 second
        c :: Time.CalendarDiffTime
        c = Time.calendarTimeTime 1

        expectation :: Time.LocalTime
        expectation = Time.ctTime c `Time.addLocalTime`
                        Time.LocalTime
                        { Time.localDay = d
                        , Time.localTimeOfDay = Time.TimeOfDay 0 0 0
                        }

testAddIntervalFromIntervalToInterval :: Test
testAddIntervalFromIntervalToInterval = do
  it "interval + interval = interval" $ testH query (`shouldBe` [expectation])
  where query :: Select (Field O.SqlInterval)
        query = pure $ (O.toFields c1 :: Field O.SqlInterval)
                       `O.addInterval`
                       (O.toFields c2 :: Field O.SqlInterval)

        -- 1 second
        c1 :: Time.CalendarDiffTime
        c1 = Time.calendarTimeTime 1

        -- 2 second
        c2 :: Time.CalendarDiffTime
        c2 = Time.calendarTimeTime 2

        expectation :: Time.CalendarDiffTime
        expectation = Time.calendarTimeTime $ Time.ctTime c1 + Time.ctTime c2

testAddIntervalFromTimestampToTimestamp :: Test
testAddIntervalFromTimestampToTimestamp = do
  it "timestamp + interval = timestamp" $ testH query (`shouldBe` [expectation])
  where query :: Select (Field O.SqlTimestamp)
        query = pure $ (O.toFields t :: Field O.SqlTimestamp)
                       `O.addInterval`
                       (O.toFields c :: Field O.SqlInterval)

        t :: Time.LocalTime
        t = Time.LocalTime
            { Time.localDay = Time.ModifiedJulianDay 0 -- = 1858-11-17
            , Time.localTimeOfDay = Time.TimeOfDay 0 0 0 -- midnight
            }

        -- 1 second
        c :: Time.CalendarDiffTime
        c = Time.calendarTimeTime 1

        expectation :: Time.LocalTime
        expectation = Time.ctTime c `Time.addLocalTime` t

testAddIntervalFromTimestamptzToTimestamptz :: Test
testAddIntervalFromTimestamptzToTimestamptz = do
  it "timestamptz + interval = timestamptz" $ testH query (`shouldBe` [expectation])
  where query :: Select (Field O.SqlTimestamptz)
        query = pure $ (O.toFields t :: Field O.SqlTimestamptz)
                       `O.addInterval`
                       (O.toFields c :: Field O.SqlInterval)

        -- UNIX epoch
        t :: Time.UTCTime
        t = Time.posixSecondsToUTCTime 0

        -- 1 second
        c :: Time.CalendarDiffTime
        c = Time.calendarTimeTime 1

        expectation :: Time.UTCTime
        expectation = Time.ctTime c `Time.addUTCTime` t

testAddIntervalFromTimeToTime :: Test
testAddIntervalFromTimeToTime = do
  it "time + interval = time" $ testH query (`shouldBe` [expectation])
  where query :: Select (Field O.SqlTime)
        query = pure $ (O.toFields t :: Field O.SqlTime)
                       `O.addInterval`
                       (O.toFields c :: Field O.SqlInterval)

        -- midnight
        t :: Time.TimeOfDay
        t = Time.TimeOfDay 0 0 0

        -- 1 second
        c :: Time.CalendarDiffTime
        c = Time.calendarTimeTime 1

        expectation :: Time.TimeOfDay
        expectation = Time.timeToTimeOfDay $
                        (realToFrac (Time.ctTime c :: Time.NominalDiffTime) :: Time.DiffTime)
                          + Time.timeOfDayToTime t

testUnnest :: Test
testUnnest = do
  it "unnest" $ testH query (`shouldBe` expectation)
  where query :: Select (Field O.SqlInt4, Field O.SqlText)
        query = O.relationValuedExpr (const expr)
          where
            expr = O.FunExpr "unnest" [O.unColumn as', O.unColumn bs']
              where
                as' :: Field (O.SqlArray O.SqlInt4)
                as' = O.toFields as
                bs' :: Field (O.SqlArray O.SqlText)
                bs' = O.toFields bs

        as :: [Int32]
        as = [1, 2, 3]

        bs :: [T.Text]
        bs = ["a", "b", "c"]

        expectation :: [(Int32, T.Text)]
        expectation = zipWith (,) as bs


main :: IO ()
main = do
  let envVarName = "POSTGRES_CONNSTRING"

  connectStringEnvVar <- lookupEnv envVarName

  connectStringDotEnv <- do vars <- Dotenv.parseFile ".env"
                            return (lookup envVarName vars)
                         `Dotenv.onMissingFile`
                         return Nothing

  let mconnectString = connectStringEnvVar <|> connectStringDotEnv

  connectString <- case mconnectString of
    Nothing ->
      fail ("Set " ++ envVarName ++ " environment variable\n"
            ++ "For example " ++ envVarName ++ "='user=tom dbname=opaleye_test "
            ++ "host=localhost port=25433 password=tom'")
    Just s -> pure (String.fromString s)

  conn <- PGS.connectPostgreSQL connectString

  dropAndCreateDB conn

  let insert (t, d) = do {
     _ <- O.runInsert_ conn O.Insert { O.iTable = t
                                     , O.iRows = d
                                     , O.iReturning = O.rCount
                                     , O.iOnConflict = Nothing }
   ; return () }

  mapM_ insert [ (table1, table1fielddata)
               , (table2, table2fielddata)
               , (table3, table3fielddata)
               , (table4, table4fielddata) ]
  insert (table6, table6fielddata)
  insert (table7, table7fielddata)
  insert (table8, table8fielddata)
  insert (table9, table9fielddata)

  PGS.close conn

  conn2 <- Connection.connectPostgreSQL connectString
  -- Need to run quickcheck after table data has been inserted
  QuickCheck.run conn2
  Connection.close conn2

  conn3 <- PGS.connectPostgreSQL connectString
  -- intervals can only be decoded to CalendarDiffTimes
  -- when the interval rendering style is set to ISO-8601.
  _nrOfAffectedRows <- PGS.execute_ conn3 "SET intervalstyle TO iso_8601;"
  hspec $ do
    before (return conn3) $ do
      describe "core dsl?" $ do
        testSelect
        testProduct
        testRestrict
        testRestrictExists
        testRestrictNotExists
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
        testValueJsonAggregate
        testByteStringJsonAggregate
        testStringJsonAggregateWithJoin
        testStringAggregate
        testOverwriteAggregateOrdered
        testMultipleAggregateOrdered
        testStringArrayAggregateOrdered
        testDistinctAndAggregate
        testDoubleAggregate
      describe "distinct" $ do
        testDistinct
      describe "distinct on"
        testDistinctOn
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
        testSingletonArray
        testArrayAppend
        testArrayPosition
        testSqlElem
      describe "joins" $ do
        testLeftJoin
        testLeftJoinNullable
        testThreeWayProduct
      describe "json" $ jsonTests table8Q
      describe "jsonb" $ do
        jsonTests table9Q
        testJsonbRightInLeft
        testJsonbLeftInRight
        testJsonbContains
        testJsonbContainsMissing
        testJsonbContainsAny
        testJsonbContainsAll
      describe "uncat" $ do
        testKeywordColNames
        testInsertSerial
        testInSelect
        testAtTimeZone
        testUnionAll
        testTableFunctor
        testValues
        testValuesEmpty
        testUpdate
        testDeleteReturning
        testInsertConflict
      describe "range" $ do
        testRangeOverlap
        testRangeDateOverlap
        testRangeLeftOf
        testRangeRightOf
        testRangeRightExtension
        testRangeLeftExtension
        testRangeAdjacency
        testRangeBoundsEnum "can access bounds from an Int8 range"
            O.sqlInt8 10 26
        testRangeBoundsEnum "can access bounds from a date range"
            O.sqlDay (read "2018-01-01") (read "2018-01-12")
      describe "literals" $ do
        testLiterals
      describe "MaybeFields" $ do
        testMaybeFieldsDistinct
      describe "Locking" $ do
        testForUpdate
      describe "Interval" $ do
        testAddIntervalFromDateToTimestamptz
        testAddIntervalFromIntervalToInterval
        testAddIntervalFromTimestampToTimestamp
        testAddIntervalFromTimestamptzToTimestamptz
        testAddIntervalFromTimeToTime
      describe "with" $ do
        testWithRecursive
        testWith
      describe "relation valued exprs" $ do
        testUnnest
