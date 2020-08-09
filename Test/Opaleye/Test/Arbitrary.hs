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
twoIntTable n = O.Table n (PP.p2 (O.required "column1", O.required "column2"))

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

recurseSafelyOneof :: [TQ.Gen a] -> [TQ.Gen a] -> [TQ.Gen a] -> TQ.Gen a
recurseSafelyOneof r0 r1 r2 =
  recurseSafely (TQ.oneof r0) (TQ.oneof r1) (TQ.oneof r2)

recurseSafely :: TQ.Gen a -> TQ.Gen a -> TQ.Gen a -> TQ.Gen a
recurseSafely r0 r1 r2 = do
    -- The range of choose is inclusive
    c <- TQ.choose (1, 10 :: Int)

    if c <= 3
    then r0
    else if c <= 8
    then r1
    else if c <= 10
    then r2
    else error "Impossible"

instance TQ.Arbitrary ArbitrarySelect where
  arbitrary = recurseSafelyOneof
                  arbitrarySelectRecurse0
                  arbitrarySelectRecurse1
                  arbitrarySelectRecurse2

instance TQ.Arbitrary ArbitrarySelectArr where
  arbitrary = recurseSafelyOneof
                  arbitrarySelectArrRecurse0
                  arbitrarySelectArrRecurse1
                  arbitrarySelectArrRecurse2

instance TQ.Arbitrary ArbitraryKleisli where
  arbitrary = recurseSafelyOneof
                  arbitraryKleisliRecurse0
                  arbitraryKleisliRecurse1
                  arbitraryKleisliRecurse2

-- It would be better if ArbitrarySelect recursively called this, but
-- it will do for now.
--
-- We are skirting close to generating infinite query territory here!
-- We should be careful about precisely how we recurse.
instance TQ.Arbitrary ArbitrarySelectMaybe where
  arbitrary = do
    TQ.oneof $
      (fmap . fmap) ArbitrarySelectMaybe $
      map (\fg -> do { ArbitrarySelect q <- TQ.arbitrary
                     ; f <- fg
                     ; return (f q)
                     })
      genSelectArrMaybeMapper
      ++
      [ do
          ArbitrarySelect q <- TQ.arbitrary
          return (fmap fieldsToMaybeFields q)
      ]
      ++
      [ do
          ArbitrarySelectMaybe qm <- TQ.arbitrary
          ArbitrarySelectArrMaybe q <- TQ.arbitrary
          return (q <<< qm)
      ]

instance TQ.Arbitrary ArbitrarySelectArrMaybe where
  arbitrary = do
    TQ.oneof $
      (fmap . fmap) ArbitrarySelectArrMaybe $
      [ do
          ArbitrarySelectMaybe q <- TQ.arbitrary
          return (P.lmap (const ()) q)
      , do
          ArbitrarySelectArr q <- TQ.arbitrary
          return (traverse' q)
      ]
    where traverse' = O.traverseMaybeFieldsExplicit unpackFields unpackFields


-- [Note] Testing strategy
--
-- We have to be very careful otherwise we will generate
-- infinite-sized expressions.  On the other hand we probably generate
-- far too small small expressions.  We should probably improve that
-- but explicitly passing a size parameter to the sub-generators.
--
-- The idea here is that only arbitrary... generators can do
-- recursion, i.e. call arbitrary in a way that could lead to other
-- calls of arbitrary.  The gen... functions don't call arbitrary
-- again, but can return functions to which arbitrary argument can be
-- applied by arbitrary... generators.

arbitrarySelectRecurse0 :: [TQ.Gen ArbitrarySelect]
arbitrarySelectRecurse0 =
  (fmap . fmap) ArbitrarySelect $
  genSelect

arbitrarySelectRecurse1 :: [TQ.Gen ArbitrarySelect]
arbitrarySelectRecurse1 =
  (fmap . fmap) ArbitrarySelect $
  -- I'm not sure this is neccessary anymore.  It should be covered by
  -- other generation pathways.
  [ do
      ArbitrarySelectArr q <- TQ.arbitrary
      return (q <<< pure emptyChoices)
  ]
  ++
  map (\fg -> do { ArbitrarySelect q <- TQ.arbitrary
                 ; f <- fg
                 ; return (f q) })
      genSelectMapper

arbitrarySelectRecurse2 :: [TQ.Gen ArbitrarySelect]
arbitrarySelectRecurse2 =
    (fmap . fmap) ArbitrarySelect $
    map (\fg -> do { ArbitrarySelect q1 <- TQ.arbitrary
                   ; ArbitrarySelect q2 <- TQ.arbitrary
                   ; f <- fg
                   ; pure (f q1 q2)
                   })
    genSelectArrPoly
    ++
    map (\fg -> do { ArbitrarySelectArr q1 <- TQ.arbitrary
                   ; ArbitrarySelect q2 <- TQ.arbitrary
                   ; f <- fg
                   ; pure (f q1 q2)
                   })
    genSelectArrMapper2
    ++
    map (\fg -> do { ArbitrarySelect q1 <- TQ.arbitrary
                   ; ArbitrarySelect q2 <- TQ.arbitrary
                   ; f <- fg
                   ; pure (f q1 q2)
                   })
    genSelectMapper2

arbitrarySelectArrRecurse0 :: [TQ.Gen ArbitrarySelectArr]
arbitrarySelectArrRecurse0 =
  (fmap . fmap) ArbitrarySelectArr $
     map (fmap ignoreArguments) genSelect
  ++ genSelectArr
  where ignoreArguments = P.lmap (const ())

arbitrarySelectArrRecurse1 :: [TQ.Gen ArbitrarySelectArr]
arbitrarySelectArrRecurse1 =
    (fmap . fmap) ArbitrarySelectArr $
    map (\fg -> do { ArbitrarySelectArr q <- TQ.arbitrary
                   ; f <- fg
                   ; pure (O.laterally f q) })
        genSelectMapper
    ++
    map (\fg -> do { ArbitrarySelectArr q <- TQ.arbitrary
                   ; f <- fg
                   ; pure (f q) })
        genSelectArrMapper
    ++
    map (\fg -> do { ArbitrarySelectArr q <- TQ.arbitrary
                   ; f <- fg
                   ; pure (fmap (Choices . pure . Right) (f q)) })
        genSelectArrMaybeMapper
    ++
    map (\fg -> do { ArbitraryKleisli q <- TQ.arbitrary
                   ; f <- fg
                   ; pure (f q) })
        [ pure O.lateral ]

arbitrarySelectArrRecurse2 :: [TQ.Gen ArbitrarySelectArr]
arbitrarySelectArrRecurse2 =
    (fmap . fmap) ArbitrarySelectArr $
    map (\fg -> do { ArbitrarySelectArr q1 <- TQ.arbitrary
                   ; ArbitrarySelectArr q2 <- TQ.arbitrary
                   ; f <- fg
                   ; pure (O.bilaterally f q1 q2) })
        genSelectMapper2
    ++
    (
    map (\fg -> do { ArbitrarySelectArr q1 <- TQ.arbitrary
                   ; ArbitrarySelectArr q2 <- TQ.arbitrary
                   ; f <- fg
                   ; pure (f q1 q2)
                   }) $
    genSelectArrPoly
    ++
    genSelectArrMapper2
    )

arbitraryKleisliRecurse0 :: [TQ.Gen ArbitraryKleisli]
arbitraryKleisliRecurse0 =
  (fmap . fmap) (ArbitraryKleisli . const) genSelect
  ++ [ pure (ArbitraryKleisli pure) ]

arbitraryKleisliRecurse1 :: [TQ.Gen ArbitraryKleisli]
arbitraryKleisliRecurse1 =
  map (\fg -> do { ArbitrarySelectArr q <- TQ.arbitrary
                 ; f <- fg
                 ; return (ArbitraryKleisli (f q)) })
  [ pure O.viaLateral ]

arbitraryKleisliRecurse2 :: [TQ.Gen ArbitraryKleisli]
arbitraryKleisliRecurse2 =
  map (\fg -> do { ArbitraryKleisli q1 <- TQ.arbitrary
                 ; ArbitraryKleisli q2 <- TQ.arbitrary
                 ; f <- fg
                 ; return (ArbitraryKleisli (f q1 q2)) })
  [ pure (<=<) , pure (liftA2 (liftA2 appendChoices)) ]

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
        l                <- TQ.choose (0, 100)
        return (O.limit l)
    , do
        l                <- TQ.choose (0, 100)
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
