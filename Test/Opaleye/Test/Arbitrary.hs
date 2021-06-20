{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Opaleye.Test.Arbitrary where

import           Prelude hiding (compare, (.), id)

import           Opaleye.Test.Fields

import qualified Opaleye as O
import qualified Opaleye.Join as OJ

import           Control.Applicative (pure, (<$>), (<*>), liftA2)
import qualified Control.Arrow as Arrow
import           Control.Arrow ((<<<))
import           Control.Category ((.), id)
import           Control.Monad ((<=<))
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Monoid as Monoid
import qualified Test.QuickCheck as TQ

data Order = Asc | Desc deriving Show

newtype ArbitrarySelect   = ArbitrarySelect (O.Select Fields)
newtype ArbitrarySelectMaybe =
  ArbitrarySelectMaybe (O.Select (O.MaybeFields Fields))
newtype ArbitrarySelectArr = ArbitrarySelectArr (O.SelectArr Fields Fields)
newtype ArbitrarySelectArrMaybe =
  ArbitrarySelectArrMaybe (O.SelectArr (O.MaybeFields Fields) (O.MaybeFields Fields))
newtype ArbitraryKleisli = ArbitraryKleisli (Fields -> O.Select Fields)
newtype ArbitraryHaskells = ArbitraryHaskells { unArbitraryHaskells :: Haskells }
                        deriving Show
newtype ArbitraryMaybeHaskells =
  ArbitraryMaybeHaskells { unArbitraryMaybeHaskells :: Maybe Haskells }
  deriving Show
newtype ArbitraryFields = ArbitraryFields { unArbitraryFields :: Fields }
newtype ArbitraryHaskellsList =
  ArbitraryHaskellsList { unArbitraryHaskellsList :: [HaskellsTuple] }
                             deriving Show
newtype ArbitraryPositiveInt = ArbitraryPositiveInt Int
                            deriving Show
newtype ArbitraryOrder = ArbitraryOrder { unArbitraryOrder :: [(Order, Int)] }
                      deriving Show
newtype ArbitraryFunction =
  ArbitraryFunction { unArbitraryFunction :: forall m i b s.
                      Functor m => Choices m i b s -> Choices m i b s }

twoIntTable :: String
            -> O.Table (O.Field O.SqlInt4, O.Field O.SqlInt4)
                       (O.Field O.SqlInt4, O.Field O.SqlInt4)
twoIntTable n = O.Table n (PP.p2 (O.requiredTableField "column1",
                                  O.requiredTableField "column2"))

table1 :: O.Table (O.Field O.SqlInt4, O.Field O.SqlInt4)
                  (O.Field O.SqlInt4, O.Field O.SqlInt4)
table1 = twoIntTable "table1"

instance TQ.Arbitrary ArbitraryFunction where
  arbitrary = do
    i <- TQ.choose (0 :: Int, 4)

    return (ArbitraryFunction (\xs ->
        if i == 0 then
          evens xs `appendChoices` odds xs
        else if i == 1 then
          evens xs `appendChoices` evens xs
        else if i == 2 then
          odds xs `appendChoices` odds xs
        else if i == 3 then
          evens xs
        else
          odds xs))

-- We don't have the ability to aggregate MaybeFields, at least, not
-- yet.  Therefore we just replace them with Nothing.
aggregateFields :: O.Aggregator Fields Fields
aggregateFields =
  -- The requirement to cast to int4 is silly, but we still have a bug
  --
  --     https://github.com/tomjaguarpaw/haskell-opaleye/issues/117
  ppChoices (choicePP (P.rmap (O.unsafeCast "int4") O.sum)
                      O.boolAnd
                      (O.stringAgg (O.sqlString ", ")))
            (const (PP.purePP (O.nothingFieldsExplicit (pure emptyChoices))))

arbitraryOrder :: ArbitraryOrder -> O.Order Fields
arbitraryOrder =
  Monoid.mconcat
  . map (\(direction, index) ->
           (case direction of
              Asc  -> \f -> chooseChoice f (O.asc id) (O.asc id) (O.asc id)
              Desc -> \f -> chooseChoice f (O.desc id) (O.desc id) (O.desc id))
           -- If the list is empty we have to conjure up an arbitrary
           -- value of type Field.  We don't know how to order
           -- MaybeFields (yet) so we do the same if we hit a
           -- MaybeFields.
           (\c -> let l = unChoices c
                      len = length l
                  in if len > 0 then
                       case l !! (index `mod` length l) of
                         Left i  -> i
                         Right _ -> CInt 0
                  else
                       CInt 0))
  . unArbitraryOrder

restrictFirstBool :: O.SelectArr Fields Fields
restrictFirstBool = proc fields -> do
  O.restrict -< fst (firstBoolOrTrue (O.sqlBool True) fields)
  Arrow.returnA -< fields

instance Show ArbitrarySelect where
  show (ArbitrarySelect q) = maybe "Empty query" id
                              (O.showSqlExplicit unpackFields q)

instance Show ArbitrarySelectMaybe where
  show (ArbitrarySelectMaybe q) =
    maybe "Empty query" id
          (O.showSqlExplicit (O.unpackspecMaybeFields unpackFields) q)

instance Show ArbitrarySelectArr where
  -- We could plug in dummy data here, or maybe just an empty list
  show _ = "ArbitrarySelectArr"

instance Show ArbitrarySelectArrMaybe where
  -- We could plug in dummy data here, or maybe just an empty list
  show _ = "ArbitrarySelectArr"

instance Show ArbitraryKleisli where
  -- We could plug in dummy data here, or maybe just an empty list
  show _ = "ArbitraryKleisli"

instance Show ArbitraryFunction where
  show = const "A function"

instance Show ArbitraryFields where
  show = const "Fields"

recurseSafelyOneof :: Int
                   -> [TQ.Gen a]
                   -> [Int -> TQ.Gen a]
                   -> [Int -> Int -> TQ.Gen a]
                   -> TQ.Gen a
recurseSafelyOneof size r0 r1 r2 =
  if size <= 1
  then TQ.oneof r0
  else TQ.oneof $
         fmap (\g -> g (size - 1)) r1
         ++ fmap (\g -> do
                     -- TQ.choose is inclusive
                     size1 <- TQ.choose (1, size - 2)
                     let size2 = size - size1 - 1
                     -- size1 and size2 are between 1 and size - 2
                     -- inclusive.  Their sum is size - 1.
                     g size1 size2) r2

arbitrarySelect :: Int -> TQ.Gen (O.Select Fields)
arbitrarySelect size =
  fmap (\case ArbitrarySelect q -> q) $
               recurseSafelyOneof
                  size
                  arbitrarySelectRecurse0
                  arbitrarySelectRecurse1
                  arbitrarySelectRecurse2

arbitrarySelectArr :: Int -> TQ.Gen (O.SelectArr Fields Fields)
arbitrarySelectArr size =
  fmap (\case ArbitrarySelectArr q -> q) $
               recurseSafelyOneof
                  size
                  arbitrarySelectArrRecurse0
                  arbitrarySelectArrRecurse1
                  arbitrarySelectArrRecurse2

arbitraryKleisli :: Int -> TQ.Gen (Fields -> O.Select Fields)
arbitraryKleisli size =
  fmap (\case ArbitraryKleisli q -> q) $
               recurseSafelyOneof
                  size
                  arbitraryKleisliRecurse0
                  arbitraryKleisliRecurse1
                  arbitraryKleisliRecurse2

arbitrarySelectMaybe :: Int -> TQ.Gen (O.Select (O.MaybeFields Fields))
arbitrarySelectMaybe size = do
  fmap (\case ArbitrarySelectMaybe q -> q) $
               recurseSafelyOneof
                  size
                  arbitrarySelectMaybeRecurse0
                  arbitrarySelectMaybeRecurse1
                  arbitrarySelectMaybeRecurse2

arbitrarySelectArrMaybe :: Int
                        -> TQ.Gen (O.SelectArr (O.MaybeFields Fields) (O.MaybeFields Fields))
arbitrarySelectArrMaybe size = do
  fmap (\case ArbitrarySelectArrMaybe q -> q) $
               recurseSafelyOneof
                  size
                  arbitrarySelectArrMaybeRecurse0
                  arbitrarySelectArrMaybeRecurse1
                  []

-- [Note] Size of expressions
--
-- 19 seems to be the biggest size we can get away with.  At 24 we see
-- a lot of errors like the below in GitHub Actions (although not
-- locally).  The Opaleye QuickCheck test process dies without
-- printing any error. One would expect QuickCheck to print an error
-- between its most recent success ("+++ OK, passed 1000 tests.") and
-- cabal-install's output ("Test suite test: FAIL").  Since QuickCheck
-- doesn't print anything I expect that the Opaleye test process must
-- be dying in a drastic way (perhaps OOM killed by the OS or perhaps
-- libpq is segfaulting).
--
-- Running 1 test suites...
-- Test suite test: RUNNING...
-- NOTICE:  table "table1" does not exist, skipping
-- NOTICE:  table "TABLE2" does not exist, skipping
-- NOTICE:  table "table3" does not exist, skipping
-- NOTICE:  table "table4" does not exist, skipping
-- NOTICE:  table "keywordtable" does not exist, skipping
-- NOTICE:  table "table6" does not exist, skipping
-- NOTICE:  table "table7" does not exist, skipping
-- NOTICE:  table "table5" does not exist, skipping
-- NOTICE:  table "table8" does not exist, skipping
-- NOTICE:  table "table10" does not exist, skipping
-- NOTICE:  table "table9" does not exist, skipping
-- +++ OK, passed 1000 tests.
-- +++ OK, passed 1000 tests.
-- +++ OK, passed 1000 tests.
-- +++ OK, passed 1000 tests.
-- +++ OK, passed 1000 tests.
-- +++ OK, passed 1000 tests.
-- Test suite test: FAIL
-- Test suite logged to:
-- /tmp/extra-dir-43542418781377/opaleye-0.7.1.0/dist-newstyle/build/x86_64-linux/ghc-8.8.4/opaleye-0.7.1.0/t/test/test/opaleye-0.7.1.0-test.log
-- 0 of 1 test suites (0 of 1 test cases) passed.
-- cabal: Tests failed for test:test from opaleye-0.7.1.0.
--
-- neil: Failed when running system command: cabal v2-exec cabal v2-test
-- CallStack (from HasCallStack):
--   error, called at src/System/Process/Extra.hs:34:9 in extra-1.7.8-ba0157cc68fafaa3027316e0961d40ff9f524d841ce1db561c650f9b1f917124:System.Process.Extra
--   system_, called at src/Cabal.hs:248:13 in main:Cabal
-- Error: Process completed with exit code 1.

instance TQ.Arbitrary ArbitrarySelect where
  arbitrary = do
    size <- TQ.choose (1, 19)
    fmap ArbitrarySelect (arbitrarySelect size)

instance TQ.Arbitrary ArbitrarySelectArr where
  arbitrary = do
    size <- TQ.choose (1, 19)
    fmap ArbitrarySelectArr (arbitrarySelectArr size)

instance TQ.Arbitrary ArbitraryKleisli where
  arbitrary = do
    size <- TQ.choose (1, 19)
    fmap ArbitraryKleisli (arbitraryKleisli size)

instance TQ.Arbitrary ArbitrarySelectMaybe where
  arbitrary = do
    size <- TQ.choose (1, 19)
    fmap ArbitrarySelectMaybe (arbitrarySelectMaybe size)

instance TQ.Arbitrary ArbitrarySelectArrMaybe where
  arbitrary = do
    size <- TQ.choose (1, 19)
    fmap ArbitrarySelectArrMaybe (arbitrarySelectArrMaybe size)

-- [Note] Testing strategy
--
-- We have to be very careful otherwise we will generate
-- infinite-sized expressions.  On the other hand we probably generate
-- far too many small expressions.  We should probably improve that
-- but explicitly passing a size parameter to the sub-generators.
--
-- The idea here is that only arbitrary... generators can do
-- recursion, i.e. call arbitrary in a way that could lead to other
-- calls of arbitrary.  The gen... functions don't call arbitrary
-- again, but can return functions to which arbitrary argument can be
-- applied by arbitrary... generators.

arbitraryG :: Functor f => (a -> b) -> [[f a]] -> [f b]
arbitraryG arbitraryC = (fmap . fmap) arbitraryC . concat

arbitrarySelectRecurse0 :: [TQ.Gen ArbitrarySelect]
arbitrarySelectRecurse0 =
  arbitraryG ArbitrarySelect
  [
  genSelect
  ]

arbitrarySelectRecurse1 :: [Int -> TQ.Gen ArbitrarySelect]
arbitrarySelectRecurse1 =
  arbitraryG (fmap ArbitrarySelect)
  [
  -- I'm not sure this is neccessary anymore.  It should be covered by
  -- other generation pathways.
  map (\fg size -> fg <*> arbitrarySelectArr size)
      [ pure (<<< pure emptyChoices) ]
  ,
  map (\fg size -> fg <*> arbitrarySelect size)
      genSelectMapper
  ]

arbitrarySelectRecurse2 :: [Int -> Int -> TQ.Gen ArbitrarySelect]
arbitrarySelectRecurse2 =
  arbitraryG ((fmap . fmap) ArbitrarySelect)
    [
    map (\fg size1 size2 -> fg <*> arbitrarySelect size1 <*> arbitrarySelect size2)
    genSelectArrPoly
    ,
    map (\fg size1 size2 -> fg <*> arbitrarySelectArr size1 <*> arbitrarySelect size2)
    genSelectArrMapper2
    ,
    map (\fg size1 size2 -> fg <*> arbitrarySelect size1 <*> arbitrarySelect size2)
    genSelectMapper2
    ]

arbitrarySelectArrRecurse0 :: [TQ.Gen ArbitrarySelectArr]
arbitrarySelectArrRecurse0 =
  arbitraryG ArbitrarySelectArr
    [
    map (fmap ignoreArguments) genSelect
    ,
    genSelectArr
    ]
  where ignoreArguments = P.lmap (const ())

arbitrarySelectArrRecurse1 :: [Int -> TQ.Gen ArbitrarySelectArr]
arbitrarySelectArrRecurse1 =
  arbitraryG (fmap ArbitrarySelectArr)
    [
    map (\fg size -> fg <*> arbitrarySelectArr size)
        ((fmap . fmap) O.laterally genSelectMapper)
    ,
    map (\fg size -> fg <*> arbitrarySelectArr size)
        genSelectArrMapper
    ,
    map (\fg size -> fg <*> arbitrarySelectArr size)
        ((fmap . fmap . fmap . fmap) (Choices . pure . Right) genSelectArrMaybeMapper)
    ,
    map (\fg size -> fg <*> arbitraryKleisli size)
        [ pure O.lateral ]
    ]

arbitrarySelectArrRecurse2 :: [Int -> Int -> TQ.Gen ArbitrarySelectArr]
arbitrarySelectArrRecurse2 =
  arbitraryG ((fmap . fmap) ArbitrarySelectArr)
    [
    map (\fg size1 size2 -> fg <*> arbitrarySelectArr size1 <*> arbitrarySelectArr size2)
        ((fmap . fmap) O.bilaterally genSelectMapper2)
    ,
    map (\fg size1 size2 -> fg <*> arbitrarySelectArr size1 <*> arbitrarySelectArr size2) $
    genSelectArrPoly
    ++
    genSelectArrMapper2
    ]

arbitraryKleisliRecurse0 :: [TQ.Gen ArbitraryKleisli]
arbitraryKleisliRecurse0 =
  arbitraryG ArbitraryKleisli
  [
  (fmap . fmap) const genSelect
  ,
  [ pure pure ]
  ]

arbitraryKleisliRecurse1 :: [Int -> TQ.Gen ArbitraryKleisli]
arbitraryKleisliRecurse1 =
  arbitraryG (fmap ArbitraryKleisli)
  [
  map (\fg size -> fg <*> arbitrarySelectArr size)
  [ pure O.viaLateral ]
  ,
  map (\fg size -> fg <*> arbitraryKleisli size)
     (map (fmap (.)) genSelectMapper)
  ]

arbitraryKleisliRecurse2 :: [Int -> Int -> TQ.Gen ArbitraryKleisli]
arbitraryKleisliRecurse2 =
  arbitraryG ((fmap . fmap) ArbitraryKleisli)
  [
  map (\fg size1 size2 -> fg <*> arbitraryKleisli size1 <*> arbitraryKleisli size2)
  [ pure (<=<) , pure (liftA2 (liftA2 appendChoices)) ]
  ]

arbitrarySelectMaybeRecurse0 :: [TQ.Gen ArbitrarySelectMaybe]
arbitrarySelectMaybeRecurse0 =
  arbitraryG ArbitrarySelectMaybe
  [ (fmap . fmap . fmap) (const (O.nothingFieldsExplicit nullspecFields)) genSelect
  , (fmap . fmap . fmap) O.justFields genSelect
  ]

arbitrarySelectMaybeRecurse1 :: [Int -> TQ.Gen ArbitrarySelectMaybe]
arbitrarySelectMaybeRecurse1 =
      (fmap . fmap . fmap) ArbitrarySelectMaybe $
      map (\fg size -> fg <*> arbitrarySelect size)
      genSelectArrMaybeMapper

arbitrarySelectMaybeRecurse2 :: [Int -> Int -> TQ.Gen ArbitrarySelectMaybe]
arbitrarySelectMaybeRecurse2 =
      (fmap . fmap . fmap . fmap) ArbitrarySelectMaybe $
      [ \size1 size2 -> do
          qm <- arbitrarySelectMaybe size1
          q <- arbitrarySelectArrMaybe size2
          return (q <<< qm)
      ]

arbitrarySelectArrMaybeRecurse0 :: [TQ.Gen ArbitrarySelectArrMaybe]
arbitrarySelectArrMaybeRecurse0 =
    arbitraryG ArbitrarySelectArrMaybe
    [ fmap (\fg -> fg <*> TQ.arbitrary)
    [ pure (Arrow.arr . fmap . unArbitraryFunction) ]
    ]

arbitrarySelectArrMaybeRecurse1 :: [Int -> TQ.Gen ArbitrarySelectArrMaybe]
arbitrarySelectArrMaybeRecurse1 =
      arbitraryG (fmap ArbitrarySelectArrMaybe)
      [
      map (\fg size -> fg <*> arbitrarySelectMaybe size)
      [ pure (P.lmap (const ())) ]
      , map (\fg size -> fg <*> arbitrarySelectArr size)
      [ pure traverse' ]
      ]
    where traverse' = O.traverseMaybeFieldsExplicit unpackFields unpackFields


genSelect :: [TQ.Gen (O.Select Fields)]
genSelect =
    [ do
        ArbitraryHaskells fields_ <- TQ.arbitrary
        return ((pure . fieldsOfHaskells) fields_)
    , return        (fmap (\(x,y) -> Choices [Left (CInt x), Left (CInt y)])
                          (O.selectTable table1))
    , do
        TQ.oneof [
            do
            ArbitraryHaskellsList l <- TQ.arbitrary
            return (fmap fieldsList (O.valuesSafe (fmap O.toFields l)))
          , -- We test empty lists of values separately, because we
            -- used to not support them
            do
              s <- TQ.choose (0, 5)
              l <- TQ.vectorOf s (pure ())
              return (fmap (const emptyChoices) (O.valuesSafe l))
          ]
    ]

genSelectArr :: [TQ.Gen (O.SelectArr Fields Fields)]
genSelectArr =
    [ do
        f                <- TQ.arbitrary
        return (Arrow.arr (unArbitraryFunction f))

    , do
        return restrictFirstBool
    ]

genSelectMapper :: [TQ.Gen (O.Select Fields -> O.Select Fields)]
genSelectMapper =
    [ do
        return (O.distinctExplicit distinctFields)
    , do
        ArbitraryPositiveInt l <- TQ.arbitrary
        return (O.limit l)
    , do
        ArbitraryPositiveInt l <- TQ.arbitrary
        return (O.offset l)
    , do
        o                <- TQ.arbitrary
        return (O.orderBy (arbitraryOrder o))

    , do
        return (O.aggregate aggregateFields)
    , do
        let q' q = P.dimap (\_ -> fst . firstBoolOrTrue (O.sqlBool True))
                           (fieldsList
                            . O.fromMaybeFields (0,
                                                 O.sqlBool True,
                                                 O.justFields (O.sqlString "field"))
                            . fmap listFields)
                           (O.optionalRestrictExplicit unpackFields q)
        return q'
    ]

genSelectMapper2 :: [TQ.Gen (O.Select Fields -> O.Select Fields
                                             -> O.Select Fields)]
genSelectMapper2 =
  [ do
      binaryOperation <- TQ.elements [ O.intersect
                                     , O.intersectAll
                                     , O.union
                                     , O.unionAll
                                     , O.except
                                     , O.exceptAll
                                     ]
      return (arbitraryBinary binaryOperation)
  ]
  where arbitraryBinary binaryOperation q1 q2 =
          (fmap fieldsList
            (binaryOperation
              (fmap listFields q1)
              (fmap listFields q2)))

genSelectArrMapper :: [TQ.Gen (O.SelectArr a Fields -> O.SelectArr a Fields)]
genSelectArrMapper =
    [ do
        thisLabel        <- TQ.arbitrary
        return (O.label thisLabel)
    ]

genSelectArrMaybeMapper :: [TQ.Gen (O.SelectArr a Fields
                                    -> O.SelectArr a (O.MaybeFields Fields))]
genSelectArrMaybeMapper =
  [ do
      return (OJ.optionalExplicit unpackFields)
  , do
      return (fmap fieldsToMaybeFields)
  ]

genSelectArrPoly :: [TQ.Gen (O.SelectArr a Fields
                             -> O.SelectArr a Fields
                             -> O.SelectArr a Fields)]
genSelectArrPoly =
    [ do
        pure (\q1 q2 -> appendChoices <$> q1 <*> q2)
    ]

genSelectArrMapper2 :: [TQ.Gen (O.SelectArr b c
                                -> O.SelectArr a b
                                -> O.SelectArr a c)]
genSelectArrMapper2 =
    [ do
        pure (<<<)
    ]

instance TQ.Arbitrary ArbitraryHaskells where
    arbitrary = arbitraryHaskells 6

instance TQ.Arbitrary ArbitraryMaybeHaskells where
    arbitrary = do
      TQ.frequency [ (1, pure (ArbitraryMaybeHaskells Nothing))
                   , (20, do
                         ArbitraryHaskells hs <- TQ.arbitrary
                         pure (ArbitraryMaybeHaskells (Just hs)))
                   ]

instance TQ.Arbitrary ArbitraryFields where
    arbitrary = (ArbitraryFields . fieldsOfHaskells . unArbitraryHaskells)
                   <$> TQ.arbitrary

-- Postgres strings cannot contain the zero codepoint.  See
--
-- https://www.postgresql.org/message-id/1171970019.3101.328.camel@coppola.muc.ecircle.de
arbitraryPGString :: TQ.Gen String
arbitraryPGString = filter (/= '\0') <$> TQ.arbitrary

arbitraryHaskells :: Int -> TQ.Gen ArbitraryHaskells
arbitraryHaskells size = do
      s <- TQ.choose (0, size)

      l <- TQ.vectorOf s (TQ.oneof
              [ Left  <$> CInt    <$> TQ.arbitrary
              , Left  <$> CBool   <$> TQ.arbitrary
              , Left  <$> CString <$> arbitraryPGString
              , pure (Right Nothing)
              , do
                  ArbitraryHaskells c <- arbitraryHaskells (size `div` 2)
                  return (Right (Just c))
              ])

      return (ArbitraryHaskells (Choices l))

instance TQ.Arbitrary ArbitraryHaskellsList where
  -- We don't want to choose very big lists because we take
  -- products of queries and so their sizes are going to end up
  -- multiplying.
  arbitrary = do
    k <- TQ.choose (0, 5)
    l <- TQ.vectorOf k $ do
      i <- TQ.arbitrary
      b <- TQ.arbitrary
      ms <- TQ.oneof [ pure Nothing
                     , Just <$> arbitraryPGString
                     ]
      pure (i, b, ms)
    return (ArbitraryHaskellsList l)

instance TQ.Arbitrary ArbitraryPositiveInt where
  arbitrary = fmap ArbitraryPositiveInt (TQ.choose (0, 100))

instance TQ.Arbitrary ArbitraryOrder where
  arbitrary = fmap ArbitraryOrder
                   (TQ.listOf ((,)
                               <$> TQ.oneof [return Asc, return Desc]
                               <*> TQ.choose (0, 100)))
