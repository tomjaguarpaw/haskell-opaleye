{-# LANGUAGE FlexibleContexts #-}

module QuickCheck where

import qualified Opaleye as O
import qualified Database.PostgreSQL.Simple as PGS
import qualified Test.QuickCheck as TQ
import           Control.Applicative (Applicative, pure, (<$>), (<*>), liftA2)
import qualified Data.Profunctor.Product.Default as D
import           Data.List (sort, sortBy)
import qualified Data.Profunctor.Product as PP
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord
import qualified Data.Set as Set

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

type Columns = [O.Column O.PGInt4]
type Haskells = [Int]

data ArbitraryQuery   = ArbitraryQuery (O.Query Columns)
data ArbitraryColumns = ArbitraryColumns { unArbitraryColumns :: Columns }
                        deriving Show
data ArbitraryPositiveInt = ArbitraryPositiveInt Int
                            deriving Show
data ArbitraryOrder = ArbitraryOrder { unArbitraryOrder :: [(Order, Int)] }
                      deriving Show

data Order = Asc | Desc deriving Show

unpackColumns :: O.Unpackspec Columns Columns
unpackColumns = PP.list D.def

instance Show ArbitraryQuery where
  show (ArbitraryQuery q) = O.showSqlForPostgresExplicit unpackColumns q

instance TQ.Arbitrary ArbitraryQuery where
  arbitrary = TQ.oneof [
      (ArbitraryQuery . pure . unArbitraryColumns)
        <$> TQ.arbitrary
    , return (ArbitraryQuery (fmap (\(x,y) -> [x,y]) (O.queryTable table1)))
    , do
        ArbitraryQuery q <- TQ.arbitrary
        return (ArbitraryQuery (O.distinctExplicit (PP.list D.def) q))
    , do
        ArbitraryQuery q <- TQ.arbitrary
        l                <- TQ.choose (0, 100)
        return (ArbitraryQuery (O.limit l q))
    , do
        ArbitraryQuery q <- TQ.arbitrary
        l                <- TQ.choose (0, 100)
        return (ArbitraryQuery (O.offset l q))
    , do
        ArbitraryQuery q <- TQ.arbitrary
        o                <- TQ.arbitrary
        return (ArbitraryQuery (O.orderBy (arbitraryOrder o) q))
    ]

instance TQ.Arbitrary ArbitraryColumns where
    arbitrary = do
    l <- TQ.listOf (TQ.oneof (map return [-1, 0, 1]))
    return (ArbitraryColumns l)

instance TQ.Arbitrary ArbitraryPositiveInt where
  arbitrary = fmap ArbitraryPositiveInt (TQ.choose (0, 100))

instance TQ.Arbitrary ArbitraryOrder where
  arbitrary = fmap ArbitraryOrder
                   (TQ.listOf ((,)
                               <$> TQ.oneof [return Asc, return Desc]
                               <*> TQ.choose (0, 100)))

arbitraryOrder :: ArbitraryOrder -> O.Order Columns
arbitraryOrder = Monoid.mconcat
                 . map (\(direction, index) ->
                         (case direction of
                             Asc  -> O.asc
                             Desc -> O.desc)
                         -- If the list is empty we have to conjure up
                         -- an arbitrary value of type Column
                         (\l -> let len = length l
                                in if len > 0 then
                                     l !! (index `mod` length l)
                                   else
                                     0))
                 . unArbitraryOrder

arbitraryOrdering :: ArbitraryOrder -> Haskells -> Haskells -> Ord.Ordering
arbitraryOrdering = Monoid.mconcat
                    . map (\(direction, index) ->
                            (case direction of
                                Asc  -> id
                                Desc -> flip)
                         -- If the list is empty we have to conjure up
                         -- an arbitrary value of type Column
                            (Ord.comparing (\l -> let len = length l
                                                  in if len > 0 then
                                                        l !! (index `mod` length l)
                                                     else
                                                        0)))
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
denotation' = denotation (PP.list D.def)

denotation2 :: O.Query (Columns, Columns)
            -> QueryDenotation (Haskells, Haskells)
denotation2 = denotation (PP.list D.def PP.***! PP.list D.def)

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

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList

distinct :: PGS.Connection -> ArbitraryQuery -> IO Bool
distinct conn (ArbitraryQuery q) = do
  compare' conn (denotation' (O.distinctExplicit (PP.list D.def) q))
                (onList nub (denotation' q))

run :: PGS.Connection -> IO ()
run conn = do
  let propApply     = (fmap . fmap) TQ.ioProperty (apply conn)
      propLimit     = (fmap . fmap) TQ.ioProperty (limit conn)
      propOffset    = (fmap . fmap) TQ.ioProperty (offset conn)
      propOrder     = (fmap . fmap) TQ.ioProperty (order conn)
      propDistinct  = fmap          TQ.ioProperty (distinct conn)

  let t p = errorIfNotSuccess =<< TQ.quickCheckResult p

  t propApply
  t propLimit
  t propOffset
  t propOrder
  t propDistinct

errorIfNotSuccess :: TQ.Result -> IO ()
errorIfNotSuccess r = case r of
  TQ.Success _ _ _ -> return ()
  _                -> error "Failed"
