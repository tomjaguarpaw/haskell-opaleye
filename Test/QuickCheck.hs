{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module QuickCheck where

import           Prelude hiding (compare, (.), id)

import           Connection (Connection, withConnection)
import           Opaleye.Test.Arbitrary
import           Opaleye.Test.Fields
import           Opaleye.Test.TraverseA (traverseA1)

import qualified Opaleye as O
import qualified Opaleye.Join as OJ
import qualified Opaleye.Exists as OE

import qualified Database.PostgreSQL.Simple as PGS
import           Control.Applicative (Applicative, pure, (<$>), (<*>))
import qualified Control.Arrow as Arrow
import           Control.Arrow ((<<<))
import           Control.Category (Category, (.), id)
import           Control.Monad (when, guard)
import qualified Data.Profunctor.Product.Default as D
import qualified Data.List as List
import qualified Data.MultiSet as MultiSet
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord hiding (compare)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Test.QuickCheck as TQ
import           Test.QuickCheck ((===), (.&&.))


arbitraryOrdering :: ArbitraryOrder -> Haskells -> Haskells -> Ord.Ordering
arbitraryOrdering =
  Monoid.mconcat
  . map (\(direction, index) ->
            (case direction of
                Asc  -> id
                Desc -> flip)
            -- If the list is empty we have to conjure up an arbitrary
            -- value of type Field.  We don't know how to order
            -- MaybeFields (yet) so we do the same if we hit a
            -- MaybeFields.
            --
            -- Note that this one will compare CInt Int
            -- to CBool Bool, but it never gets asked to
            -- do so, so we don't care.
            (Ord.comparing (\c -> let l = unChoices c
                                      len = length l
                                  in if len > 0 then
                                       case l !! (index `mod` length l) of
                                         Left i  -> i
                                         Right _ -> CInt 0
                                  else
                                       CInt 0)))
  . unArbitraryOrder

newtype SelectArrDenotation a b =
  SelectArrDenotation { unSelectArrDenotation :: PGS.Connection -> a -> IO [b] }

type SelectDenotation = SelectArrDenotation ()

instance Functor (SelectArrDenotation a) where
  fmap = onList . fmap

instance Applicative (SelectArrDenotation a) where
  pure = Arrow.arr . const
  f <*> x = fmap (uncurry ($)) (f Arrow.&&& x)

instance Category SelectArrDenotation where
  id = Arrow.arr id
  (.) = \(SelectArrDenotation f) (SelectArrDenotation g) ->
          SelectArrDenotation (\conn a -> do
                                  bs <- g conn a
                                  concatMapM (f conn) bs)

instance Arrow.Arrow SelectArrDenotation where
  arr f = SelectArrDenotation (\_ -> pure . pure . f)
  first f = SelectArrDenotation (\conn (a, b) -> do
                                    as' <- unSelectArrDenotation f conn a
                                    pure (fmap (\a' -> (a', b)) as'))

instance Arrow.ArrowChoice SelectArrDenotation where
  left f = SelectArrDenotation $ \conn -> \case
    Left l -> (fmap . fmap) Left (unSelectArrDenotation f conn l)
    Right r -> (pure . pure . pure) r

runSelectArrDenotation :: SelectArrDenotation a b
                       -> a
                       -> PGS.Connection
                       -> IO [b]
runSelectArrDenotation sab a conn = unSelectArrDenotation sab conn a

($$) :: Arrow.Arrow arr => arr a b -> a -> arr () b
($$) f a = f <<< Arrow.arr (const a)

unApply :: (a -> SelectDenotation b) -> SelectArrDenotation a b
unApply f = SelectArrDenotation (\conn a -> unSelectArrDenotation (f a) conn ())

onList :: ([a] -> [b]) -> SelectArrDenotation i a -> SelectArrDenotation i b
onList f = SelectArrDenotation . (fmap . fmap . fmap) f . unSelectArrDenotation

onListK :: (a -> [b]) -> SelectArrDenotation a b
onListK f = SelectArrDenotation (\_ a -> pure (f a))

-- This is taking liberties.  Firstly it errors out when two fields
-- are of different types.  It should probably return a Maybe or an
-- Either.  Secondly, it doesn't detect when lists are the same
-- length and it probably should.
--
-- We don't have the ability to aggregate MaybeFields, at least, not
-- yet.  Therefore we just replace them with Nothing.
aggregateDenotation :: [Haskells] -> [Haskells]
aggregateDenotation cs = if null cs
                         then []
                         else (pure
                              . List.foldl1' combine
                              . map emptyOutChoices
                              ) cs
  where combine h1 h2 = Choices (zipWith (curry (\case
          (Left l1, Left l2) -> Left $ case (l1, l2) of
            (CInt  i1, CInt i2)  -> CInt (i1 + i2)
            (CBool b1, CBool b2) -> CBool (b1 && b2)
            (CString s1, CString s2) -> CString (s1 ++ ", " ++ s2)
            _ -> error "Impossible"
          (Right _, Right _) -> Right Nothing
          _ -> error "Impossible")) (unChoices h1) (unChoices h2))

        emptyOutChoices c = Choices $ flip map (unChoices c) $ \case
            Left l  -> Left l
            Right _ -> Right Nothing

optionalDenotation :: SelectArrDenotation i a -> SelectArrDenotation i (Maybe a)
optionalDenotation = onList $ \case
  [] -> [Nothing]
  xs -> map Just xs

lateralDenotation :: (a -> SelectDenotation r) -> SelectArrDenotation a r
lateralDenotation = unApply

optionalRestrictDenotation :: SelectDenotation Haskells
                           -> SelectArrDenotation (Haskells -> Bool) (Maybe Haskells)
optionalRestrictDenotation hs = optionalDenotation $ proc cond -> do
  a <- hs -< ()
  onListK guard -< cond a
  Arrow.returnA -< a

pureList :: [a] -> SelectDenotation a
pureList = SelectArrDenotation . pure . pure . pure

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

denotationArrExplicit :: (ah -> af)
                      -> O.FromFields bf bh
                      -> O.SelectArr af bf
                      -> SelectArrDenotation ah bh
denotationArrExplicit toFields qr q =
  SelectArrDenotation (\conn h ->
      let fs = pure (toFields h)
      in O.runSelectExplicit qr conn (q <<< fs))

denotationExplicit :: O.FromFields fields a
                   -> O.Select fields
                   -> SelectDenotation a
denotationExplicit = denotationArrExplicit O.toFields

denotation :: O.Select Fields -> SelectDenotation Haskells
denotation = denotationExplicit fromFieldsFields

denotationArr :: O.SelectArr Fields Fields
              -> SelectArrDenotation Haskells Haskells
denotationArr = denotationArrExplicit fieldsOfHaskells fromFieldsFields

denotationMaybeFields :: O.Select (O.MaybeFields Fields)
                      -> SelectDenotation (Maybe Haskells)
denotationMaybeFields =
  denotationExplicit (O.fromFieldsMaybeFields fromFieldsFields)

denotationArrMaybeFields
  :: O.SelectArr (O.MaybeFields Fields) (O.MaybeFields Fields)
  -> SelectArrDenotation (Maybe Haskells) (Maybe Haskells)
denotationArrMaybeFields =
  denotationArrExplicit
    (O.toFieldsExplicit (O.toFieldsMaybeFields nullspecFields toFieldsFields))
    (O.fromFieldsMaybeFields fromFieldsFields)

unSelectDenotations :: SelectDenotation a
                    -> SelectDenotation b
                    -> ([a] -> [b] -> IO TQ.Property)
                    -> Connection
                    -> IO TQ.Property
unSelectDenotations one two = unSelectArrDenotations one two ()

unSelectArrDenotations :: SelectArrDenotation i a
                       -> SelectArrDenotation i b
                       -> i
                       -> ([a] -> [b] -> IO TQ.Property)
                       -> Connection
                       -> IO TQ.Property
unSelectArrDenotations one two i k conn = do
  withConnection conn (runSelectArrDenotation one i) >>= \case
    Left _ -> discard
    Right oner -> withConnection conn (runSelectArrDenotation two i) >>= \case
      Left _ -> discard
      Right twor -> k oner twor

  where discard = do
          putStrLn "A denotation failed to run but it was not our fault"
          pure (TQ.property TQ.Discard)

-- { Comparing the results

-- compareNoSort is stronger than compare so prefer to use it where
-- possible.  If the queries do not compare equal but do compare equal
-- sorted then switch to "compare".  That's no big deal.
compareNoSort :: (Ord a, Show a)
              => SelectDenotation a
              -> SelectDenotation a
              -> Connection
              -> IO TQ.Property
compareNoSort one two =
  unSelectDenotations one two $ \one' two' -> do
  when (one' /= two')
       (putStrLn $ if List.sort one' == List.sort two'
                   then "[but they are equal sorted]"
                   else "AND THEY'RE NOT EVEN EQUAL SORTED!")

  return (one' === two')

compare :: (Show a, Ord a)
         => SelectDenotation a
         -> SelectDenotation a
         -> Connection
         -> IO TQ.Property
compare one two = unSelectDenotations one two $ \one' two' ->
  return (List.sort one' === List.sort two')

-- The condition is *not* that denotation of the sort is equal to sort
-- of the denotation, because the ordering may not fully specify the
-- order.
compareSortedBy :: (Show a, Ord a)
                => (a -> a -> Ord.Ordering)
                -> SelectDenotation a
                -> SelectDenotation a
                -> Connection
                -> IO TQ.Property
compareSortedBy o one two = unSelectDenotations one two $ \one' two' ->
  return ((List.sort one' === List.sort two')
          .&&. isSortedBy o one')

type ArbitraryArgument = ArbitraryHaskells

denotationC :: Arrow.Arrow selectArr
            => (SelectDenotation Haskells -> selectArr () b -> t1 -> t2)
            -> O.SelectArr Fields Fields
            -> selectArr Haskells b
            -> ArbitraryHaskells
            -> t1
            -> t2
denotationC compare_ q d (ArbitraryHaskells h) conn =
  compare_ (denotation (q . pure (fieldsOfHaskells h))) (d $$ h) conn

compareDenotation :: O.SelectArr Fields Fields
                  -> SelectArrDenotation Haskells Haskells
                  -> ArbitraryArgument
                  -> Connection
                  -> IO TQ.Property
compareDenotation = denotationC compare

compareDenotationMaybe :: O.SelectArr (O.MaybeFields Fields) Fields
                       -> SelectArrDenotation (Maybe Haskells) Haskells
                       -> ArbitraryMaybeHaskells
                       -> Connection
                       -> IO TQ.Property
compareDenotationMaybe q d (ArbitraryMaybeHaskells h) =
  compare (denotation (q . pure (fieldsOfMaybeHaskells h))) (d $$ h)

compareDenotationMaybe2 :: O.SelectArr (O.MaybeFields Fields)
                                       (O.MaybeFields Fields)
                        -> SelectArrDenotation (Maybe Haskells) (Maybe Haskells)
                        -> ArbitraryMaybeHaskells
                        -> Connection
                        -> IO TQ.Property
compareDenotationMaybe2 q d (ArbitraryMaybeHaskells h) =
  compare (denotationMaybeFields (q . pure (fieldsOfMaybeHaskells h)))
          (d $$ h)

compareDenotationNoSort :: O.SelectArr Fields Fields
                        -> SelectArrDenotation Haskells Haskells
                        -> ArbitraryArgument
                        -> Connection
                        -> IO TQ.Property
compareDenotationNoSort = denotationC compareNoSort

compareDenotation' :: (O.Select Fields -> O.Select Fields)
                   -> ([Haskells] -> [Haskells])
                   -> ArbitrarySelect
                   -> Connection
                   -> IO TQ.Property
compareDenotation' f g (ArbitrarySelect q) =
  compare (denotation (f q)) (onList g (denotation q))

compareDenotationMaybe' :: (O.Select Fields -> O.Select (O.MaybeFields Fields))
                        -> (SelectDenotation Haskells -> SelectDenotation (Maybe Haskells))
                        -> ArbitrarySelect
                        -> Connection
                        -> IO TQ.Property
compareDenotationMaybe' f g (ArbitrarySelect q) =
  compare (denotationMaybeFields (f q)) (g (denotation q))

compareDenotationNoSort' :: (O.Select Fields -> O.Select Fields)
                         -> ([Haskells] -> [Haskells])
                         -> ArbitrarySelect
                         -> Connection
                         -> IO TQ.Property
compareDenotationNoSort' f g (ArbitrarySelect q) =
  compareNoSort (denotation (f q)) (onList g (denotation q))

-- }

-- { The tests

fields :: ArbitraryHaskells -> ArbitraryArgument -> Connection -> IO TQ.Property
       -- ^ The ArbitraryArgument isn't really used
fields (ArbitraryHaskells c) =
  compareDenotationNoSort (pure (fieldsOfHaskells c)) (pure c)

compose :: ArbitrarySelectArr
        -> ArbitrarySelectArr
        -> ArbitraryArgument
        -> Connection
        -> IO TQ.Property
compose (ArbitrarySelectArr q1) (ArbitrarySelectArr q2) = do
  compareDenotation (q1 . q2) (denotationArr q1 . denotationArr q2)

-- We need to test this separately otherwise traverseMaybeFields won't
-- be properly tested.  We need that [traverseMaybeFields q <<< q'] is
-- [traverseMaybeFields q] <<< [q] and we only test it when q' is of
-- the form pure h.  The 'compose' tests do the rest, therefore we
-- need a version of 'compose' that checks composition of things whose
-- types are 'MaybeFields Fields'.
composeMaybe :: ArbitrarySelectArrMaybe
             -> ArbitrarySelectArrMaybe
             -> ArbitraryMaybeHaskells
             -> Connection
             -> IO TQ.Property
composeMaybe (ArbitrarySelectArrMaybe q1) (ArbitrarySelectArrMaybe q2) = do
  compareDenotationMaybe2 (q1 . q2) (denotation_ q1 . denotation_ q2)
  where denotation_ = denotationArrMaybeFields

identity :: ArbitraryArgument
         -> Connection
         -> IO TQ.Property
identity = compareDenotation id id

arr :: ArbitraryFunction
    -> ArbitraryArgument
    -> Connection
    -> IO TQ.Property
arr (ArbitraryFunction f) =
  compareDenotationNoSort (Arrow.arr f) (Arrow.arr f)

fmap' :: ArbitraryFunction
      -> ArbitrarySelectArr
      -> ArbitraryArgument
      -> Connection
      -> IO TQ.Property
fmap' (ArbitraryFunction f) (ArbitrarySelectArr q) =
  compareDenotationNoSort (fmap f q)
                          (fmap f (denotationArr q))

apply :: ArbitrarySelectArr
      -> ArbitrarySelectArr
      -> ArbitraryArgument
      -> Connection
      -> IO TQ.Property
apply (ArbitrarySelectArr q1) (ArbitrarySelectArr q2) =
  compareDenotation (pair <$> q1 <*> q2)
                    (pair <$> denotationArr q1 <*> denotationArr q2)

  where pair x y = Choices [Right (pure x), Right (pure y)]


-- When combining arbitrary queries with the applicative product <*>
-- the limit of the denotation is not always the denotation of the
-- limit.  Without some ordering applied before the limit the returned
-- rows can vary.  If an ordering is applied beforehand we can check
-- the invariant that the returned rows always compare smaller than
-- the remainder under the applied ordering.
--
-- Strangely the same caveat doesn't apply to offset.
limit :: ArbitraryPositiveInt
      -> ArbitrarySelect
      -> ArbitraryOrder
      -> Connection
      -> IO TQ.Property
limit (ArbitraryPositiveInt l) (ArbitrarySelect q) o = do
  let limited = O.limit l (O.orderBy (arbitraryOrder o) q)

  unSelectDenotations (denotation limited) (denotation q) $ \limited' unlimited' -> do
      let remainder = MultiSet.fromList unlimited'
                      `MultiSet.difference`
                      MultiSet.fromList limited'
          maxChosen :: Maybe Haskells
          maxChosen = maximumBy (arbitraryOrdering o) limited'
          minRemain :: Maybe Haskells
          minRemain = minimumBy (arbitraryOrdering o) (MultiSet.toList remainder)
          cond :: Maybe Bool
          cond = lteBy (arbitraryOrdering o) <$> maxChosen <*> minRemain
          condBool :: Bool
          condBool = Maybe.fromMaybe True cond

      return ((length limited' === min l (length unlimited'))
              .&&. condBool)

offset :: ArbitraryPositiveInt -> ArbitrarySelect -> Connection
       -> IO TQ.Property
offset (ArbitraryPositiveInt n) =
  compareDenotationNoSort' (O.offset n) (drop n)

order :: ArbitraryOrder -> ArbitrarySelect -> Connection -> IO TQ.Property
order o (ArbitrarySelect q) =
  compareSortedBy (arbitraryOrdering o)
                  (denotation (O.orderBy (arbitraryOrder o) q))
                  (denotation q)

distinct :: ArbitrarySelect -> Connection -> IO TQ.Property
distinct =
  compareDenotation' (O.distinctExplicit distinctFields) nub

-- When we generalise compareDenotation... we can just test
--
--    compareDenotation... conn restrict restrictDenotation
restrict :: ArbitraryArgument -> Connection -> IO TQ.Property
restrict =
  compareDenotationNoSort restrictFirstBool restrictFirstBoolDenotation

values :: ArbitraryHaskellsList -> Connection -> IO TQ.Property
values (ArbitraryHaskellsList l) =
  compareNoSort (denotation (fmap fieldsList (O.valuesSafe (fmap O.toFields l))))
                (pureList (fmap fieldsList l))

-- We test values entries of length two in values, and values entries
-- of length zero here.  Ideally we would find some way to merge them.
valuesEmpty :: [()] -> Connection -> IO TQ.Property
valuesEmpty l =
  compareNoSort (denotationExplicit D.def (O.valuesSafe l))
                (pureList l)

aggregate :: ArbitrarySelect -> Connection -> IO TQ.Property
aggregate =
  compareDenotationNoSort' (O.aggregate aggregateFields)
                           aggregateDenotation


label :: String -> ArbitrarySelect -> Connection -> IO TQ.Property
label comment = compareDenotationNoSort' (O.label comment) id

optional :: ArbitrarySelect -> Connection -> IO TQ.Property
optional = compareDenotationMaybe' (OJ.optionalExplicit unpackFields)
                                   optionalDenotation

optionalRestrict :: ArbitrarySelect -> Connection -> IO TQ.Property
optionalRestrict =
  compareDenotationMaybe' optionalRestrictF optionalRestrictFDenotation
  where optionalRestrictF = f (firstBoolOrTrue (O.sqlBool True))
                              (O.optionalRestrictExplicit unpackFields)

        optionalRestrictFDenotation = f (firstBoolOrTrue True)
                                        optionalRestrictDenotation

        f x y z = Arrow.arr (\() -> fst . x) Arrow.>>> (y z)

maybeFieldsToSelect :: ArbitraryMaybeHaskells -> Connection -> IO TQ.Property
maybeFieldsToSelect =
  compareDenotationMaybe O.maybeFieldsToSelect
                         (onListK Maybe.maybeToList)

traverseMaybeFields :: ArbitrarySelectArr
                    -> ArbitraryMaybeHaskells
                    -> Connection
                    -> IO TQ.Property
traverseMaybeFields (ArbitrarySelectArr q) =
  compareDenotationMaybe2 (traverse' q)
                          (traverseA1 (denotationArr q))
  where traverse' = O.traverseMaybeFieldsExplicit unpackFields unpackFields

lateral :: ArbitraryKleisli
        -> ArbitraryArgument
        -> Connection
        -> IO TQ.Property
lateral (ArbitraryKleisli f) =
  compareDenotation (O.lateral f) (lateralDenotation (denotation . f'))
  where f' = f . fieldsOfHaskells

exists :: ArbitrarySelect -> Connection -> IO TQ.Property
exists = compareDenotationNoSort' (existsQ OE.exists) (existsQ existsList)
  where existsList l = [not (null l)]
        existsQ existsf q = do
          exists_ <- existsf q
          pure (Choices [Left (CBool exists_)])


{- TODO

  * Nullability
  * Operators (mathematical, logical, etc.)

-}

-- }

-- { Running the QuickCheck

-- One way that the property tests can fail is because of LIMIT and
-- OFFSET.  It seems that a query returning LIMIT or OFFSET does not
-- always return the same result when it is part of a larger query.
-- This happens rarely.  We could sort before LIMIT or OFFSET to make
-- it even rarer.

run :: Connection -> IO ()
run conn = do
  let prop1 :: TQ.Testable prop
            => (a -> Connection -> IO prop) -> a -> TQ.Property
      prop1 = fmap (\p -> TQ.ioProperty (p conn))

      prop2 :: TQ.Testable prop
            => (a1 -> a2 -> Connection -> IO prop)
            -> a1 -> a2 -> TQ.Property
      prop2 = (fmap . fmap) (\p -> TQ.ioProperty (p conn))


      prop3 :: TQ.Testable prop
            => (a1 -> a2 -> a3 -> Connection -> IO prop)
            -> a1 -> a2 -> a3 -> TQ.Property
      prop3 = (fmap . fmap . fmap) (\p -> TQ.ioProperty (p conn))

      test1 :: (Show a, TQ.Arbitrary a, TQ.Testable prop)
               => (a -> Connection -> IO prop) -> IO ()
      test1 = t . prop1

      test2 :: (Show a1, Show a2, TQ.Arbitrary a1, TQ.Arbitrary a2,
                TQ.Testable prop)
               => (a1 -> a2 -> Connection -> IO prop) -> IO ()
      test2 = t . prop2

      test3 :: (Show a1, Show a2, Show a3,
                TQ.Arbitrary a1, TQ.Arbitrary a2, TQ.Arbitrary a3,
                TQ.Testable prop)
               => (a1 -> a2 -> a3 -> Connection -> IO prop) -> IO ()
      test3 = t . prop3

      t p = errorIfNotSuccess
        =<< TQ.quickCheckWithResult (TQ.stdArgs { TQ.maxSuccess = 1000 }) p

  test1 identity
  test2 arr
  test3 compose
  test3 composeMaybe
  test2 fields
  test3 fmap'
  test3 apply
  test3 limit
  test2 offset
  test2 order
  test1 distinct
  test1 restrict
  test1 values
  test1 valuesEmpty
  test1 aggregate
  test2 label
  test1 optional
  test1 optionalRestrict
  test1 maybeFieldsToSelect
  test2 traverseMaybeFields
  test2 lateral
  test1 exists

-- }

-- { Utilities

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList

-- Replace this with `isSuccess` when the following issue is fixed
--
--     https://github.com/nick8325/quickcheck/issues/220
errorIfNotSuccess :: TQ.Result -> IO ()
errorIfNotSuccess r = case r of
  TQ.Success {} -> return ()
  _             -> error "Failed"

restrictFirstBoolDenotation :: SelectArrDenotation Haskells Haskells
restrictFirstBoolDenotation = proc hs -> do
  onListK guard -< fst (firstBoolOrTrue True hs)
  Arrow.returnA -< hs

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
