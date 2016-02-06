{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module QuickCheck where

import qualified Opaleye as O
import qualified Database.PostgreSQL.Simple as PGS
import qualified Test.QuickCheck as TQ
import           Control.Applicative (Applicative, pure, (<$>), (<*>), liftA2)
import qualified Data.Profunctor.Product.Default as D
import           Data.List (sort)
import qualified Data.List as List
import qualified Data.MultiSet as MultiSet
import qualified Data.Profunctor.Product as PP
import qualified Data.Functor.Contravariant.Divisible as Divisible
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Control.Arrow as Arrow

twoIntTable :: String
            -> O.Table (O.Column O.PGInt4, O.Column O.PGInt4)
                       (O.Column O.PGInt4, O.Column O.PGInt4)
twoIntTable n = O.Table n (PP.p2 (O.required "column1", O.required "column2"))

table1 :: O.Table (O.Column O.PGInt4, O.Column O.PGInt4)
                  (O.Column O.PGInt4, O.Column O.PGInt4)
table1 = twoIntTable "table1"

data QueryDenotation a =
  QueryDenotation { unQueryDenotation :: PGS.Connection -> IO [a] }

onList :: ([a] -> [b]) -> QueryDenotation a -> QueryDenotation b
onList f = QueryDenotation . (fmap . fmap) f . unQueryDenotation

type Columns = [Either (O.Column O.PGInt4) (O.Column O.PGBool)]
type Haskells = [Either Int Bool]

columnsOfHaskells :: Haskells -> Columns
columnsOfHaskells = O.constantExplicit eitherPP

newtype ArbitraryQuery   = ArbitraryQuery (O.Query Columns)
newtype ArbitraryColumns = ArbitraryColumns { unArbitraryColumns :: Haskells }
                        deriving Show
newtype ArbitraryColumnsList = ArbitraryColumnsList { unArbitraryColumnsList :: [Int] }
                             deriving Show
newtype ArbitraryPositiveInt = ArbitraryPositiveInt Int
                            deriving Show
newtype ArbitraryOrder = ArbitraryOrder { unArbitraryOrder :: [(Order, Int)] }
                      deriving Show
newtype ArbitraryGarble =
  ArbitraryGarble { unArbitraryGarble :: forall a. [a] -> [a] }

data Order = Asc | Desc deriving Show

unpackColumns :: O.Unpackspec Columns Columns
unpackColumns = eitherPP

instance Show ArbitraryQuery where
  show (ArbitraryQuery q) = O.showSqlForPostgresExplicit unpackColumns q

instance Show ArbitraryGarble where
  show = const "A permutation"

instance TQ.Arbitrary ArbitraryQuery where
  arbitrary = TQ.oneof [
      (ArbitraryQuery . pure . columnsOfHaskells . unArbitraryColumns)
        <$> TQ.arbitrary
    , do
        ArbitraryQuery q1 <- TQ.arbitrary
        ArbitraryQuery q2 <- TQ.arbitrary
        aq ((++) <$> q1 <*> q2)
    , return (ArbitraryQuery (fmap (\(x,y) -> [Left x, Left y]) (O.queryTable table1)))
    , do
        ArbitraryQuery q <- TQ.arbitrary
        aq (O.distinctExplicit eitherPP q)
    , do
        ArbitraryQuery q <- TQ.arbitrary
        l                <- TQ.choose (0, 100)
        aq (O.limit l q)
    , do
        ArbitraryQuery q <- TQ.arbitrary
        l                <- TQ.choose (0, 100)
        aq (O.offset l q)
    , do
        ArbitraryQuery q <- TQ.arbitrary
        o                <- TQ.arbitrary
        aq (O.orderBy (arbitraryOrder o) q)

    , do
        ArbitraryQuery q <- TQ.arbitrary
        f                <- TQ.arbitrary
        aq (fmap (unArbitraryGarble f) q)

    , do
        ArbitraryQuery q <- TQ.arbitrary
        aq (restrictFirstBool Arrow.<<< q)
    , do
        ArbitraryColumnsList l <- TQ.arbitrary
        aq (fmap (return . Left) (O.values (fmap O.constant l)))
    ]
    where aq = return . ArbitraryQuery


instance TQ.Arbitrary ArbitraryColumns where
    arbitrary = do
    l <- TQ.listOf (TQ.oneof (map (return . Left) [-1, 0, 1]
                             ++ map (return . Right) [False, True]))
    return (ArbitraryColumns l)

instance TQ.Arbitrary ArbitraryColumnsList where
  -- We don't want to choose very big lists because we take
  -- products of queries and so their sizes are going to end up
  -- multiplying.
  arbitrary = do
    k <- TQ.choose (0, 5)
    l <- TQ.vectorOf k TQ.arbitrary
    return (ArbitraryColumnsList l)

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

instance TQ.Arbitrary ArbitraryGarble where
  arbitrary = do
    i <- TQ.choose (0 :: Int, 4)

    return (ArbitraryGarble (\xs ->
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

arbitraryOrder :: ArbitraryOrder -> O.Order Columns
arbitraryOrder = Monoid.mconcat
                 . map (\(direction, index) ->
                         (case direction of
                             Asc  -> (\f -> Divisible.choose  f (O.asc id) (O.asc id))
                             Desc -> (\f -> Divisible.choose  f (O.desc id) (O.desc id)))
                         -- If the list is empty we have to conjure up
                         -- an arbitrary value of type Column
                         (\l -> let len = length l
                                in if len > 0 then
                                     l !! (index `mod` length l)
                                   else
                                     Left 0))
                 . unArbitraryOrder

arbitraryOrdering :: ArbitraryOrder -> Haskells -> Haskells -> Ord.Ordering
arbitraryOrdering = Monoid.mconcat
                    . map (\(direction, index) ->
                            (case direction of
                                Asc  -> id
                                Desc -> flip)
                         -- If the list is empty we have to conjure up
                         -- an arbitrary value of type Column
                         --
                         -- Note that this one will compare Left Int
                         -- to Right Bool, but it never gets asked to
                         -- do so, so we don't care.
                            (Ord.comparing (\l -> let len = length l
                                                  in if len > 0 then
                                                        l !! (index `mod` length l)
                                                     else
                                                        Left 0)))
                    . unArbitraryOrder

instance Functor QueryDenotation where
  fmap f = QueryDenotation . (fmap . fmap . fmap) f .unQueryDenotation

pureList :: [a] -> QueryDenotation a
pureList = QueryDenotation . pure . pure

instance Applicative QueryDenotation where
  pure    = QueryDenotation . pure . pure . pure
  f <*> x = QueryDenotation ((liftA2 . liftA2 . liftA2) ($)
                                (unQueryDenotation f) (unQueryDenotation x))

denotation :: O.QueryRunner columns a -> O.Query columns -> QueryDenotation a
denotation qr q = QueryDenotation (\conn -> O.runQueryExplicit qr conn q)

denotation' :: O.Query Columns -> QueryDenotation Haskells
denotation' = denotation eitherPP

denotation2 :: O.Query (Columns, Columns)
            -> QueryDenotation (Haskells, Haskells)
denotation2 = denotation (eitherPP PP.***! eitherPP)

-- { Comparing the results

-- compareNoSort is stronger than compare' so prefer to use it where possible
compareNoSort :: Eq a
              => PGS.Connection
              -> QueryDenotation a
              -> QueryDenotation a
              -> IO Bool
compareNoSort conn one two = do
  one' <- unQueryDenotation one conn
  two' <- unQueryDenotation two conn
  return (one' == two')

compare' :: Ord a
         => PGS.Connection
         -> QueryDenotation a
         -> QueryDenotation a
         -> IO Bool
compare' conn one two = do
  one' <- unQueryDenotation one conn
  two' <- unQueryDenotation two conn
  return (sort one' == sort two')

compareSortedBy :: Ord a
                => (a -> a -> Ord.Ordering)
                -> PGS.Connection
                -> QueryDenotation a
                -> QueryDenotation a
                -> IO Bool
compareSortedBy o conn one two = do
  one' <- unQueryDenotation one conn
  two' <- unQueryDenotation two conn
  return ((sort one' == sort two')
          && (isSortedBy o one'))

-- }

-- { The tests

columns :: PGS.Connection -> ArbitraryColumns -> IO Bool
columns conn (ArbitraryColumns c) =
  compareNoSort conn (denotation' (pure (columnsOfHaskells c)))
                     (pure c)

fmap' :: PGS.Connection -> ArbitraryGarble -> ArbitraryQuery -> IO Bool
fmap' conn f (ArbitraryQuery q) = do
  compareNoSort conn (denotation' (fmap (unArbitraryGarble f) q))
                     (onList (fmap (unArbitraryGarble f)) (denotation' q))

apply :: PGS.Connection -> ArbitraryQuery -> ArbitraryQuery -> IO Bool
apply conn (ArbitraryQuery q1) (ArbitraryQuery q2) = do
  compare' conn (denotation2 ((,) <$> q1 <*> q2))
                ((,) <$> denotation' q1 <*> denotation' q2)

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
      -> ArbitraryQuery
      -> ArbitraryOrder
      -> IO Bool
limit conn (ArbitraryPositiveInt l) (ArbitraryQuery q) o = do
  let q' = O.limit l (O.orderBy (arbitraryOrder o) q)

  one' <- unQueryDenotation (denotation' q') conn
  two' <- unQueryDenotation (denotation' q) conn

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

offset :: PGS.Connection -> ArbitraryPositiveInt -> ArbitraryQuery -> IO Bool
offset conn (ArbitraryPositiveInt l) (ArbitraryQuery q) = do
  compareNoSort conn (denotation' (O.offset l q))
                     (onList (drop l) (denotation' q))

order :: PGS.Connection -> ArbitraryOrder -> ArbitraryQuery -> IO Bool
order conn o (ArbitraryQuery q) = do
  compareSortedBy (arbitraryOrdering o)
                  conn
                  (denotation' (O.orderBy (arbitraryOrder o) q))
                  (denotation' q)

distinct :: PGS.Connection -> ArbitraryQuery -> IO Bool
distinct conn (ArbitraryQuery q) = do
  compare' conn (denotation' (O.distinctExplicit eitherPP q))
                (onList nub (denotation' q))

-- When we added <*> to the arbitrary queries we started getting some
-- consequences to do with the order of the returned rows and so
-- restrict had to start being compared sorted.
restrict :: PGS.Connection -> ArbitraryQuery -> IO Bool
restrict conn (ArbitraryQuery q) = do
  compare' conn (denotation' (restrictFirstBool Arrow.<<< q))
                (onList restrictFirstBoolList (denotation' q))

values :: PGS.Connection -> ArbitraryColumnsList -> IO Bool
values conn (ArbitraryColumnsList l) = do
  compareNoSort conn (denotation' (fmap (return . Left) (O.values (fmap O.constant l))))
                     (pureList (fmap (return . Left) l))

{- TODO

  * Aggregation
  * Binary operations
      * union
      * unionAll
      * intersect
      * intersectAll
      * except
      * exceptAll
  * Nullability
  * Left join
  * Label (check it has no effect)
  * Operators (mathematical, logical, etc.)
  * >>>?

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

      t p = errorIfNotSuccess =<< TQ.quickCheckWithResult (TQ.stdArgs { TQ.maxSuccess = 1000 }) p

  test1 columns
  test2 fmap'
  test2 apply
  test3 limit
  test2 offset
  test2 order
  test1 distinct
  test1 restrict
  test1 values

-- }

-- { Utilities

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList

eitherPP :: (D.Default p a a', D.Default p b b',
             PP.SumProfunctor p, PP.ProductProfunctor p)
         => p [Either a b] [Either a' b']
eitherPP = PP.list (D.def PP.+++! D.def)

errorIfNotSuccess :: TQ.Result -> IO ()
errorIfNotSuccess r = case r of
  TQ.Success _ _ _ -> return ()
  _                -> error "Failed"

firstBoolOrTrue :: b -> [Either a b] -> (b, [Either a b])
firstBoolOrTrue true c = (b, c)
  where b = case Maybe.mapMaybe isBool c of
          []    -> true
          (x:_) -> x

isBool :: Either a b
       -> Maybe b
isBool (Left _)  = Nothing
isBool (Right l) = Just l

restrictFirstBool :: O.QueryArr Columns Columns
restrictFirstBool = Arrow.arr snd
      Arrow.<<< Arrow.first O.restrict
      Arrow.<<< Arrow.arr (firstBoolOrTrue (O.pgBool True))

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
