{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module QuickCheck where

import qualified Opaleye as O
import qualified Database.PostgreSQL.Simple as PGS
import qualified Test.QuickCheck as TQ
import           Control.Applicative (Applicative, pure, (<$>), (<*>), liftA2)
import qualified Data.Profunctor.Product.Default as D
import           Data.List (sort, sortBy)
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

newtype ArbitraryQuery   = ArbitraryQuery (O.Query Columns)
newtype ArbitraryColumns = ArbitraryColumns { unArbitraryColumns :: Columns }
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
      (ArbitraryQuery . pure . unArbitraryColumns)
        <$> TQ.arbitrary
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
    ]
    where aq = return . ArbitraryQuery


instance TQ.Arbitrary ArbitraryColumns where
    arbitrary = do
    l <- TQ.listOf (TQ.oneof (map (return . Left) [-1, 0, 1]
                             ++ map (return . Right) [O.pgBool False, O.pgBool True]))
    return (ArbitraryColumns l)

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

-- }

-- { The tests

fmap' :: PGS.Connection -> ArbitraryGarble -> ArbitraryQuery -> IO Bool
fmap' conn f (ArbitraryQuery q) = do
  compareNoSort conn (denotation' (fmap (unArbitraryGarble f) q))
                     (onList (fmap (unArbitraryGarble f)) (denotation' q))

apply :: PGS.Connection -> ArbitraryQuery -> ArbitraryQuery -> IO Bool
apply conn (ArbitraryQuery q1) (ArbitraryQuery q2) = do
  compare' conn (denotation2 ((,) <$> q1 <*> q2))
                ((,) <$> denotation' q1 <*> denotation' q2)

limit :: PGS.Connection -> ArbitraryPositiveInt -> ArbitraryQuery -> IO Bool
limit conn (ArbitraryPositiveInt l) (ArbitraryQuery q) = do
  compareNoSort conn (denotation' (O.limit l q))
                     (onList (take l) (denotation' q))

offset :: PGS.Connection -> ArbitraryPositiveInt -> ArbitraryQuery -> IO Bool
offset conn (ArbitraryPositiveInt l) (ArbitraryQuery q) = do
  compareNoSort conn (denotation' (O.offset l q))
                     (onList (drop l) (denotation' q))

order :: PGS.Connection -> ArbitraryOrder -> ArbitraryQuery -> IO Bool
order conn o (ArbitraryQuery q) = do
  compareNoSort conn (denotation' (O.orderBy (arbitraryOrder o) q))
                     (onList (sortBy (arbitraryOrdering o)) (denotation' q))

distinct :: PGS.Connection -> ArbitraryQuery -> IO Bool
distinct conn (ArbitraryQuery q) = do
  compare' conn (denotation' (O.distinctExplicit eitherPP q))
                (onList nub (denotation' q))

restrict :: PGS.Connection -> ArbitraryQuery -> IO Bool
restrict conn (ArbitraryQuery q) = do
  compareNoSort conn (denotation' (restrictFirstBool Arrow.<<< q))
                     (onList restrictFirstBoolList (denotation' q))

-- }

-- { Running the QuickCheck

run :: PGS.Connection -> IO ()
run conn = do
  let propFmap      = (fmap . fmap) TQ.ioProperty (fmap' conn)
      propApply     = (fmap . fmap) TQ.ioProperty (apply conn)
      propLimit     = (fmap . fmap) TQ.ioProperty (limit conn)
      propOffset    = (fmap . fmap) TQ.ioProperty (offset conn)
      propOrder     = (fmap . fmap) TQ.ioProperty (order conn)
      propDistinct  = fmap          TQ.ioProperty (distinct conn)
      propRestrict  = fmap          TQ.ioProperty (restrict conn)

  let t p = errorIfNotSuccess =<< TQ.quickCheckWithResult (TQ.stdArgs { TQ.maxSuccess = 1000 }) p

  t propFmap
  t propApply
  t propLimit
  t propOffset
  t propOrder
  t propDistinct
  t propRestrict

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

-- }
