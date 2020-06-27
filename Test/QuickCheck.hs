{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Arrows #-}

module QuickCheck where

import           Prelude hiding (compare)
import qualified Opaleye as O
import qualified Opaleye.Internal.Lateral as OL
import qualified Opaleye.Join as OJ
import qualified Opaleye.ToFields as O
import           Wrapped (constructor, asSumProfunctor,
                          constructorDecidable, asDecidable)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Test.QuickCheck as TQ
import           Test.QuickCheck ((===))
import           Control.Applicative (Applicative, pure, (<$>), (<*>), liftA2)
import qualified Data.Profunctor.Product.Default as D
import           Data.List (sort)
import qualified Data.List as List
import qualified Data.MultiSet as MultiSet
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Functor.Contravariant.Divisible as Divisible
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord hiding (compare)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Control.Arrow as Arrow
import           Control.Arrow ((<<<))

twoIntTable :: String
            -> O.Table (O.Field O.SqlInt4, O.Field O.SqlInt4)
                       (O.Field O.SqlInt4, O.Field O.SqlInt4)
twoIntTable n = O.Table n (PP.p2 (O.required "column1", O.required "column2"))

table1 :: O.Table (O.Field O.SqlInt4, O.Field O.SqlInt4)
                  (O.Field O.SqlInt4, O.Field O.SqlInt4)
table1 = twoIntTable "table1"

newtype SelectArrDenotation a b =
  SelectArrDenotation { unSelectArrDenotation :: PGS.Connection -> a -> IO [b] }

type SelectDenotation = SelectArrDenotation ()

unSelectDenotation :: SelectDenotation b -> PGS.Connection -> IO [b]
unSelectDenotation sa conn = unSelectArrDenotation sa conn ()

onList :: ([a] -> [b]) -> SelectDenotation a -> SelectDenotation b
onList f = SelectArrDenotation . (fmap . fmap . fmap) f . unSelectArrDenotation

data Choice i b s = CInt i | CBool b | CString s deriving (Show, Eq, Ord)

chooseChoice :: Divisible.Decidable f
             => (a -> Choice i b s) -> f i -> f b -> f s -> f a
chooseChoice choose fi fb fs = asDecidable $ proc a -> case choose a of
  CInt i    -> constructorDecidable fi -< i
  CBool b   -> constructorDecidable fb -< b
  CString s -> constructorDecidable fs -< s

type Fields = [Choice (O.Field O.SqlInt4) (O.Field O.SqlBool) (O.Field O.SqlText)]
type Haskells = [Choice Int Bool String]

fieldsOfHaskells :: Haskells -> Fields
fieldsOfHaskells = O.toFieldsExplicit defChoicesPP

fieldsList :: (a, b) -> [Choice a b s]
fieldsList (x, y) = [CInt x, CBool y]

listFieldsG :: [Choice i b s] -> i -> b -> (i, b)
listFieldsG f i b = (fst (firstIntOr i f), fst (firstBoolOrTrue b f))

listFields :: Fields -> (O.Field O.SqlInt4, O.Field O.SqlBool)
listFields f = listFieldsG f 1 (O.sqlBool True)

listHaskells :: Haskells -> (Int, Bool)
listHaskells f = listFieldsG f 1 True

newtype ArbitrarySelect   = ArbitrarySelect (O.Select Fields)
newtype ArbitrarySelectMaybeFields =
  ArbitrarySelectMaybeFields (O.Select (O.MaybeFields Fields))
newtype ArbitrarySelectArr = ArbitrarySelectArr (O.SelectArr Fields Fields)
newtype ArbitrarySelectArrMaybeFields =
  ArbitrarySelectArrMaybeFields (O.SelectArr Fields (O.MaybeFields Fields))
newtype ArbitraryFields = ArbitraryFields { unArbitraryFields :: Haskells }
                        deriving Show
newtype ArbitraryFieldsList =
  ArbitraryFieldsList { unArbitraryFieldsList :: [(Int, Bool)] }
                             deriving Show
newtype ArbitraryPositiveInt = ArbitraryPositiveInt Int
                            deriving Show
newtype ArbitraryOrder = ArbitraryOrder { unArbitraryOrder :: [(Order, Int)] }
                      deriving Show
newtype ArbitraryFunction =
  ArbitraryFunction { unArbitraryFunction :: forall a. [a] -> [a] }

data Order = Asc | Desc deriving Show

unpackFields :: O.Unpackspec Fields Fields
unpackFields = defChoicesPP

aggregateFields :: O.Aggregator Fields Fields
aggregateFields =
  -- The requirement to cast to int4 is silly, but we still have a bug
  --
  --     https://github.com/tomjaguarpaw/haskell-opaleye/issues/117
  PP.list (choicePP (P.rmap (O.unsafeCast "int4") O.sum)
                    O.boolAnd
                    (O.stringAgg (O.sqlString ", ")))

aggregateLaterally :: O.Aggregator b b'
                   -> O.SelectArr i (Fields, b)
                   -> O.SelectArr i (Fields, b')
aggregateLaterally agg q = proc i -> do
  (a, b) <- q -< i

  b' <- OL.lateral
    (\(a, b) ->
        let aLateralInt :: O.Field O.SqlInt4
            aLateralInt = fst (firstIntOr 0 a)
        in (O.aggregateOrdered (O.asc (const aLateralInt)) agg) (pure b))
            -< (a, b)
  Arrow.returnA -< (a, b')

-- This is taking liberties.  Firstly it errors out when two fields
-- are of different types.  It should probably return a Maybe or an
-- Either.  Secondly, it doesn't detect when lists are the same
-- length and it probably should.
aggregateDenotation :: [Haskells] -> [Haskells]
aggregateDenotation cs = if null cs then [] else pure (List.foldl1' combine cs)
  where combine = zipWith (curry (\case
          (CInt  i1, CInt i2)  -> CInt (i1 + i2)
          (CBool b1, CBool b2) -> CBool (b1 && b2)
          (CString s1, CString s2) -> CString (s1 ++ ", " ++ s2)
          _ -> error "Impossible"))

optionalDenotation :: [Haskells] -> [Maybe Haskells]
optionalDenotation = \case
  [] -> [Nothing]
  xs -> map Just xs

optionalRestrictDenotation :: [Haskells] -> [Maybe Haskells]
optionalRestrictDenotation = optionalDenotation . restrictFirstBoolList

instance Show ArbitrarySelect where
  show (ArbitrarySelect q) = maybe "Empty query" id
                              (O.showSqlExplicit unpackFields q)

instance Show ArbitrarySelectMaybeFields where
  show (ArbitrarySelectMaybeFields q) =
    maybe "Empty query" id
          (O.showSqlExplicit (O.unpackspecMaybeFields unpackFields) q)

instance Show ArbitraryFunction where
  show = const "A function"

instance TQ.Arbitrary ArbitrarySelect where
  arbitrary = do
    ArbitrarySelectArr q <- TQ.arbitrary
    return (ArbitrarySelect (q <<< pure []))

instance TQ.Arbitrary ArbitrarySelectMaybeFields where
  arbitrary = do
    ArbitrarySelectArrMaybeFields q <- TQ.arbitrary
    return (ArbitrarySelectMaybeFields (q <<< pure []))

instance TQ.Arbitrary ArbitrarySelectArr where
  arbitrary = TQ.oneof [
      do
        ArbitraryFields fields_ <- TQ.arbitrary
        aqArg ((pure . fieldsOfHaskells) fields_)
    , aqArg (P.lmap (const ())
                    (fmap (\(x,y) -> [CInt x, CInt y]) (O.selectTable table1)))
    , do
        ArbitraryFieldsList l <- TQ.arbitrary
        aqArg (P.lmap (const ())
                      (fmap fieldsList (O.valuesSafe (fmap O.toFields l))))
    , do
        ArbitrarySelectArr q1 <- TQ.arbitrary
        ArbitrarySelectArr q2 <- TQ.arbitrary
        q <- TQ.oneof [ pure ((++) <$> q1 <*> q2)
                      , pure (q1 <<< q2) ]
        aqArg q
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        aq (O.distinctExplicit defChoicesPP) q
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        l                <- TQ.choose (0, 100)
        aq (O.limit l) q
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        l                <- TQ.choose (0, 100)
        aq (O.offset l) q
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        o                <- TQ.arbitrary
        aq (O.orderBy (arbitraryOrder o)) q

    , do
        f                <- TQ.arbitrary
        aqArg (Arrow.arr (unArbitraryFunction f))

    , do
        aqArg restrictFirstBool
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        aq (O.aggregate aggregateFields) q
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        thisLabel        <- TQ.arbitrary
        aqArg (O.label thisLabel q)
    , do
        ArbitrarySelectArr q1 <- TQ.arbitrary
        ArbitrarySelectArr q2 <- TQ.arbitrary
        binaryOperation <- TQ.elements [ O.intersect
                                       , O.intersectAll
                                       , O.union
                                       , O.unionAll
                                       , O.except
                                       , O.exceptAll
                                       ]
        q <- arbitraryBinary binaryOperation q1 q2
        aqArg q
    , -- This is stupidly simple way of generating lateral subqueries.
      -- All it does is run a lateral aggregation.
      do
        ArbitrarySelectArr q <- TQ.arbitrary
        aqArg ((fmap unpairColums
                . aggregateLaterally aggregateFields
                . fmap pairColumns) q)
    , do
        ArbitrarySelectArrMaybeFields q <- TQ.arbitrary

        aqArg (fmap (fieldsList
                     . O.fromMaybeFields (0, O.sqlBool True)
                     . fmap listFields)
                    q)
    ]
    where -- Applies qf to the query, but uses [] for the input of
          -- query, and ignores the input of the result.
          aq qf = aqArg . OL.laterally qf

          aqArg = return . ArbitrarySelectArr

          arbitraryBinary binaryOperation q1 q2 =
            return (fmap fieldsList
                    (OL.bilaterally binaryOperation
                     (fmap listFields q1)
                     (fmap listFields q2)))

instance TQ.Arbitrary ArbitrarySelectArrMaybeFields where
  arbitrary = TQ.oneof [
      do
        ArbitrarySelectArr q <- TQ.arbitrary
        aqArg (fmap pure q)
    , do
        ArbitraryFunction f <- TQ.arbitrary
        ArbitrarySelectArrMaybeFields q <- TQ.arbitrary
        aqArg (fmap (fmap f) q)
    , do
        ArbitrarySelectArrMaybeFields q1 <- TQ.arbitrary
        ArbitrarySelectArrMaybeFields q2 <- TQ.arbitrary
        aqArg (liftA2 (++) <$> q1 <*> q2)
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        aqArg (OJ.optionalExplicit defChoicesPP q)
    , do
        ArbitrarySelect q <- TQ.arbitrary
        let q' = P.lmap (\_ -> fst . firstBoolOrTrue (O.sqlBool True))
                        (O.optionalRestrictExplicit defChoicesPP q)
        aqArg q'
    , do
        ArbitrarySelectArr q <- TQ.arbitrary
        ArbitrarySelectArrMaybeFields qm <- TQ.arbitrary
        aqArg (O.traverseMaybeFieldsExplicit defChoicesPP defChoicesPP q <<< qm)
    ]
    where aqArg = return . ArbitrarySelectArrMaybeFields

instance TQ.Arbitrary ArbitraryFields where
    arbitrary = do
      -- Postgres strings cannot contain the zero codepoint.  See
      --
      -- https://www.postgresql.org/message-id/1171970019.3101.328.camel@coppola.muc.ecircle.de
      let arbitraryPGString = filter (/= '\0') <$> TQ.arbitrary

      l <- TQ.listOf (TQ.oneof [ CInt    <$> TQ.arbitrary
                               , CBool   <$> TQ.arbitrary
                               , CString <$> arbitraryPGString ])

      return (ArbitraryFields l)

instance TQ.Arbitrary ArbitraryFieldsList where
  -- We don't want to choose very big lists because we take
  -- products of queries and so their sizes are going to end up
  -- multiplying.
  arbitrary = do
    k <- TQ.choose (0, 5)
    l <- TQ.vectorOf k TQ.arbitrary
    return (ArbitraryFieldsList l)

instance TQ.Arbitrary ArbitraryPositiveInt where
  arbitrary = fmap ArbitraryPositiveInt (TQ.choose (0, 100))

instance TQ.Arbitrary ArbitraryOrder where
  arbitrary = fmap ArbitraryOrder
                   (TQ.listOf ((,)
                               <$> TQ.oneof [return Asc, return Desc]
                               <*> TQ.choose (0, 100)))

odds :: [a] -> [a]
odds []     = []
odds (x:xs) = x : evens xs

evens :: [a] -> [a]
evens []     = []
evens (_:xs) = odds xs

pairColumns :: [a] -> ([a], [a])
pairColumns cs = (evens cs, odds cs)

unpairColums :: ([a], [a]) -> [a]
unpairColums = uncurry (++)

instance TQ.Arbitrary ArbitraryFunction where
  arbitrary = do
    i <- TQ.choose (0 :: Int, 4)

    return (ArbitraryFunction (\xs ->
        if i == 0 then
          evens xs ++ odds xs
        else if i == 1 then
          evens xs ++ evens xs
        else if i == 2 then
          odds xs ++ odds xs
        else if i == 3 then
          evens xs
        else
          odds xs))

arbitraryOrder :: ArbitraryOrder -> O.Order Fields
arbitraryOrder =
  Monoid.mconcat
  . map (\(direction, index) ->
           (case direction of
              Asc  -> \f -> chooseChoice f (O.asc id) (O.asc id) (O.asc id)
              Desc -> \f -> chooseChoice f (O.desc id) (O.desc id) (O.desc id))
           -- If the list is empty we have to conjure up
           -- an arbitrary value of type Field
           (\l -> let len = length l
                  in if len > 0 then
                       l !! (index `mod` length l)
                  else
                       CInt 0))
  . unArbitraryOrder

arbitraryOrdering :: ArbitraryOrder -> Haskells -> Haskells -> Ord.Ordering
arbitraryOrdering =
  Monoid.mconcat
  . map (\(direction, index) ->
            (case direction of
                Asc  -> id
                Desc -> flip)
            -- If the list is empty we have to conjure up
            -- an arbitrary value of type Field
            --
            -- Note that this one will compare CInt Int
            -- to CBool Bool, but it never gets asked to
            -- do so, so we don't care.
            (Ord.comparing (\l -> let len = length l
                                  in if len > 0 then
                                       l !! (index `mod` length l)
                                  else
                                       CInt 0)))
  . unArbitraryOrder

instance Functor (SelectArrDenotation a) where
  fmap f = SelectArrDenotation
           . (fmap . fmap . fmap . fmap) f
           . unSelectArrDenotation

pureList :: [a] -> SelectDenotation a
pureList = SelectArrDenotation . pure . pure . pure

instance Applicative (SelectArrDenotation a) where
  pure    = SelectArrDenotation . pure . pure . pure . pure
  f <*> x = SelectArrDenotation ((liftA2 . liftA2 . liftA2 . liftA2) ($)
                                   (unSelectArrDenotation f)
                                   (unSelectArrDenotation x))

denotationExplicit :: O.FromFields fields a
                   -> O.Select fields
                   -> SelectDenotation a
denotationExplicit qr q =
  SelectArrDenotation (\conn () -> O.runSelectExplicit qr conn q)

denotation :: O.Select Fields -> SelectDenotation Haskells
denotation = denotationExplicit defChoicesPP

denotation2 :: O.Select (Fields, Fields)
            -> SelectDenotation (Haskells, Haskells)
denotation2 = denotationExplicit (defChoicesPP PP.***! defChoicesPP)

denotationMaybeFields :: O.Select (O.MaybeFields Fields)
                      -> SelectDenotation (Maybe Haskells)
denotationMaybeFields = denotationExplicit (O.fromFieldsMaybeFields defChoicesPP)

-- { Comparing the results

-- compareNoSort is stronger than compare so prefer to use it where possible
compareNoSort :: (Ord a, Show a)
              => PGS.Connection
              -> SelectDenotation a
              -> SelectDenotation a
              -> IO TQ.Property
compareNoSort conn one two = do
  one' <- unSelectDenotation one conn
  two' <- unSelectDenotation two conn

  if (one' /= two')
    then (putStrLn $ if (sort one' == sort two')
                     then "[but they are equal sorted]"
                     else "AND THEY'RE NOT EVEN EQUAL SORTED!")
    else return ()

  return (one' === two')

compare :: Ord a
         => PGS.Connection
         -> SelectDenotation a
         -> SelectDenotation a
         -> IO Bool
compare conn one two = do
  one' <- unSelectDenotation one conn
  two' <- unSelectDenotation two conn
  return (sort one' == sort two')

compareSortedBy :: Ord a
                => (a -> a -> Ord.Ordering)
                -> PGS.Connection
                -> SelectDenotation a
                -> SelectDenotation a
                -> IO Bool
compareSortedBy o conn one two = do
  one' <- unSelectDenotation one conn
  two' <- unSelectDenotation two conn
  return ((sort one' == sort two')
          && isSortedBy o one')

-- }

-- { The tests

fields :: PGS.Connection -> ArbitraryFields -> IO TQ.Property
fields conn (ArbitraryFields c) =
  compareNoSort conn (denotation (pure (fieldsOfHaskells c)))
                     (pure c)

fmap' :: PGS.Connection -> ArbitraryFunction -> ArbitrarySelect -> IO TQ.Property
fmap' conn f (ArbitrarySelect q) =
  compareNoSort conn (denotation (fmap (unArbitraryFunction f) q))
                     (onList (fmap (unArbitraryFunction f)) (denotation q))

apply :: PGS.Connection -> ArbitrarySelect -> ArbitrarySelect -> IO Bool
apply conn (ArbitrarySelect q1) (ArbitrarySelect q2) =
  compare conn (denotation2 ((,) <$> q1 <*> q2))
                ((,) <$> denotation q1 <*> denotation q2)

-- When combining arbitrary queries with the applicative product <*>
-- the limit of the denotation is not always the denotation of the
-- limit.  Without some ordering applied before the limit the returned
-- rows can vary.  If an ordering is applied beforehand we can check
-- the invariant that the returned rows always compare smaller than
-- the remainder under the applied ordering.
--
-- Strangely the same caveat doesn't apply to offset.
limit :: PGS.Connection
      -> ArbitraryPositiveInt
      -> ArbitrarySelect
      -> ArbitraryOrder
      -> IO Bool
limit conn (ArbitraryPositiveInt l) (ArbitrarySelect q) o = do
  let q' = O.limit l (O.orderBy (arbitraryOrder o) q)

  one' <- unSelectDenotation (denotation q') conn
  two' <- unSelectDenotation (denotation q) conn

  let remainder = MultiSet.fromList two'
                  `MultiSet.difference`
                  MultiSet.fromList one'
      maxChosen :: Maybe Haskells
      maxChosen = maximumBy (arbitraryOrdering o) one'
      minRemain :: Maybe Haskells
      minRemain = minimumBy (arbitraryOrdering o) (MultiSet.toList remainder)
      cond :: Maybe Bool
      cond = lteBy (arbitraryOrdering o) <$> maxChosen <*> minRemain
      condBool :: Bool
      condBool = Maybe.fromMaybe True cond

  return ((length one' == min l (length two'))
          && condBool)

offset :: PGS.Connection -> ArbitraryPositiveInt -> ArbitrarySelect
       -> IO TQ.Property
offset conn (ArbitraryPositiveInt l) (ArbitrarySelect q) =
  compareNoSort conn (denotation (O.offset l q))
                     (onList (drop l) (denotation q))

order :: PGS.Connection -> ArbitraryOrder -> ArbitrarySelect -> IO Bool
order conn o (ArbitrarySelect q) =
  compareSortedBy (arbitraryOrdering o)
                  conn
                  (denotation (O.orderBy (arbitraryOrder o) q))
                  (denotation q)

distinct :: PGS.Connection -> ArbitrarySelect -> IO Bool
distinct conn (ArbitrarySelect q) =
  compare conn (denotation (O.distinctExplicit defChoicesPP q))
                (onList nub (denotation q))

-- When we added <*> to the arbitrary queries we started getting some
-- consequences to do with the order of the returned rows and so
-- restrict had to start being compared sorted.
restrict :: PGS.Connection -> ArbitrarySelect -> IO Bool
restrict conn (ArbitrarySelect q) =
  compare conn (denotation (restrictFirstBool <<< q))
                (onList restrictFirstBoolList (denotation q))

values :: PGS.Connection -> ArbitraryFieldsList -> IO TQ.Property
values conn (ArbitraryFieldsList l) =
  compareNoSort conn
                (denotation (fmap fieldsList (O.valuesSafe (fmap O.toFields l))))
                (pureList (fmap fieldsList l))

aggregate :: PGS.Connection -> ArbitrarySelect -> IO TQ.Property
aggregate conn (ArbitrarySelect q) =
  compareNoSort conn (denotation (O.aggregate aggregateFields q))
                     (onList aggregateDenotation (denotation q))


label :: PGS.Connection -> String -> ArbitrarySelect -> IO TQ.Property
label conn comment (ArbitrarySelect q) =
  compareNoSort conn (denotation (O.label comment q))
                     (denotation q)

optional :: PGS.Connection -> ArbitrarySelect -> IO Bool
optional conn (ArbitrarySelect q) =
  compare conn (denotationMaybeFields (OJ.optionalExplicit defChoicesPP q))
                (onList optionalDenotation (denotation q))

optionalRestrict :: PGS.Connection -> ArbitrarySelect -> IO Bool
optionalRestrict conn (ArbitrarySelect q) =
  compare conn (denotationMaybeFields q1)
                (onList optionalRestrictDenotation (denotation q))
  where q1 = P.lmap (\() -> fst . firstBoolOrTrue (O.sqlBool True))
                    (O.optionalRestrictExplicit defChoicesPP q)

-- This isn't a very thorough test, but traverseMaybeFields returns a
-- SelectArr so testing it has the same problem as testing <<<.  We
-- need to test a denotation that is a *function*, rather than just a
-- list.  We haven't implemented that yet.  It would require running a
-- base query and doing one trip to the database for each row in the
-- base query, applying the SelectArr, and then concatenating the
-- returned lists.
traverseMaybeFields :: PGS.Connection
                    -> ArbitrarySelectMaybeFields
                    -> IO Bool
traverseMaybeFields conn (ArbitrarySelectMaybeFields q) =
  compare conn
           (denotationMaybeFields
              (O.traverseMaybeFieldsExplicit defChoicesPP defChoicesPP f <<< q))
           (onList (traverse f' =<<) (denotationMaybeFields q))
  where f = proc l -> do
          O.restrict -< (fst . firstBoolOrTrue (O.sqlBool True)) l
          Arrow.returnA -< l
        f' = \h -> if (fst . firstBoolOrTrue True) h then [h] else []


{- TODO

  * Nullability
  * Operators (mathematical, logical, etc.)
  * Denotation of <<<

-}

-- }

-- { Running the QuickCheck

run :: PGS.Connection -> IO ()
run conn = do
  let prop1 p = fmap          TQ.ioProperty (p conn)
      prop2 p = (fmap . fmap) TQ.ioProperty (p conn)
      prop3 p = (fmap . fmap . fmap) TQ.ioProperty (p conn)

      test1 :: (Show a, TQ.Arbitrary a, TQ.Testable prop)
               => (PGS.Connection -> a -> IO prop) -> IO ()
      test1 = t . prop1

      test2 :: (Show a1, Show a2, TQ.Arbitrary a1, TQ.Arbitrary a2,
                TQ.Testable prop)
               => (PGS.Connection -> a1 -> a2 -> IO prop) -> IO ()
      test2 = t . prop2

      test3 :: (Show a1, Show a2, Show a3,
                TQ.Arbitrary a1, TQ.Arbitrary a2, TQ.Arbitrary a3,
                TQ.Testable prop)
               => (PGS.Connection -> a1 -> a2 -> a3 -> IO prop) -> IO ()
      test3 = t . prop3

      t p = errorIfNotSuccess
        =<< TQ.quickCheckWithResult (TQ.stdArgs { TQ.maxSuccess = 1000 }) p

  test1 fields
  test2 fmap'
  test2 apply
  test3 limit
  test2 offset
  test2 order
  test1 distinct
  test1 restrict
  test1 values
  test1 aggregate
  test2 label
  test1 optional
  test1 optionalRestrict
  test1 traverseMaybeFields

-- }

-- { Utilities

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList

choicePP :: PP.SumProfunctor p
         => p i1 i2 -> p b1 b2 -> p s1 s2
         -> p (Choice i1 b1 s1) (Choice i2 b2 s2)
choicePP p1 p2 p3 = asSumProfunctor $ proc choice -> case choice of
  CInt i    -> constructor CInt    p1 -< i
  CBool b   -> constructor CBool   p2 -< b
  CString s -> constructor CString p3 -< s

defChoicesPP :: (D.Default p a a', D.Default p b b', D.Default p s s',
                 PP.SumProfunctor p, PP.ProductProfunctor p)
             => p [Choice a b s] [Choice a' b' s']
defChoicesPP = PP.list (choicePP D.def D.def D.def)

-- Replace this with `isSuccess` when the following issue is fixed
--
--     https://github.com/nick8325/quickcheck/issues/220
errorIfNotSuccess :: TQ.Result -> IO ()
errorIfNotSuccess r = case r of
  TQ.Success {} -> return ()
  _             -> error "Failed"

firstBoolOrTrue :: b -> [Choice a b s] -> (b, [Choice a b s])
firstBoolOrTrue true c = (b, c)
  where b = case Maybe.mapMaybe isBool c of
          []    -> true
          (x:_) -> x

firstIntOr :: a -> [Choice a b s] -> (a, [Choice a b s])
firstIntOr else_ c = (b, c)
  where b = case Maybe.mapMaybe isInt c of
          []    -> else_
          (x:_) -> x

isBool :: Choice a b s
       -> Maybe b
isBool (CInt _)  = Nothing
isBool (CBool l) = Just l
isBool (CString _) = Nothing

isInt :: Choice a b s -> Maybe a
isInt (CInt a)  = Just a
isInt (CBool _) = Nothing
isInt (CString _) = Nothing

restrictFirstBool :: O.SelectArr Fields Fields
restrictFirstBool = Arrow.arr snd
      <<< Arrow.first O.restrict
      <<< Arrow.arr (firstBoolOrTrue (O.sqlBool True))

restrictFirstBoolList :: [Haskells] -> [Haskells]
restrictFirstBoolList = map snd
                        . filter fst
                        . map (firstBoolOrTrue True)

isSortedBy ::(a -> a -> Ord.Ordering) -> [a] -> Bool
isSortedBy comp xs = all (uncurry (.<=)) (zip xs (tail' xs))
  where tail' []     = []
        tail' (_:ys) = ys
        x .<= y       = lteBy comp x y

lteBy :: (a -> a -> Ord.Ordering) -> a -> a -> Bool
lteBy comp x y = comp x y /= Ord.GT

maximumBy :: (a -> a -> Ord.Ordering) -> [a] -> Maybe a
maximumBy _ []       = Nothing
maximumBy c xs@(_:_) = Just (List.maximumBy c xs)

minimumBy :: (a -> a -> Ord.Ordering) -> [a] -> Maybe a
minimumBy = maximumBy . flip

-- }
