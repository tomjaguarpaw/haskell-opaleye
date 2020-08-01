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

import qualified Opaleye as O
import qualified Opaleye.Join as OJ

import qualified Database.PostgreSQL.Simple as PGS
import           Control.Applicative (Applicative, pure, (<$>), (<*>), liftA2)
import qualified Control.Arrow as Arrow
import           Control.Arrow ((<<<))
import           Control.Category (Category, (.), id)
import           Control.Monad (when)
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Either
import qualified Data.List as List
import qualified Data.MultiSet as MultiSet
import qualified Data.Profunctor as P
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
  pure    = SelectArrDenotation . pure . pure . pure . pure
  f <*> x = SelectArrDenotation ((liftA2 . liftA2 . liftA2 . liftA2) ($)
                                   (unSelectArrDenotation f)
                                   (unSelectArrDenotation x))

instance Category SelectArrDenotation where
  id = SelectArrDenotation (\_ -> pure . pure)
  (.) = \(SelectArrDenotation f) (SelectArrDenotation g) ->
          SelectArrDenotation (\conn a -> do
                                  bs <- g conn a
                                  concatMapM (f conn) bs)

runSelectArrDenotation :: SelectArrDenotation a b
                       -> a
                       -> PGS.Connection
                       -> IO [b]
runSelectArrDenotation sab a conn = unSelectArrDenotation sab conn a

onList :: ([a] -> [b]) -> SelectArrDenotation i a -> SelectArrDenotation i b
onList f = SelectArrDenotation . (fmap . fmap . fmap) f . unSelectArrDenotation

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

optionalDenotation :: [Haskells] -> [Maybe Haskells]
optionalDenotation = \case
  [] -> [Nothing]
  xs -> map Just xs

optionalRestrictDenotation :: [Haskells] -> [Maybe Haskells]
optionalRestrictDenotation = optionalDenotation . restrictFirstBoolList

traverseDenotation :: SelectArrDenotation a Haskells
                   -> SelectDenotation (Maybe a)
                   -> SelectDenotation (Maybe Haskells)
traverseDenotation (SelectArrDenotation f) (SelectArrDenotation q) =
  (SelectArrDenotation (\conn l -> do
                           qr <- q conn l
                           let nothings :: [()]
                               (nothings, justs) =
                                 Data.Either.partitionEithers
                                   (map (\case
                                            Nothing -> Left ()
                                            Just j -> Right j)
                                        qr)

                           justs' <- concatMapM (f conn) justs
                           let _ = justs' :: [Haskells]

                           return ((Just <$> justs')
                                   ++ (Nothing <$ nothings))))

lateralDenotation :: (a -> SelectDenotation r)
                  -> SelectArrDenotation a r
lateralDenotation f = SelectArrDenotation (\conn l ->
    (\r -> unSelectArrDenotation (f r) conn ()) l)

pureList :: [a] -> SelectDenotation a
pureList = SelectArrDenotation . pure . pure . pure

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

denotationExplicit :: O.FromFields fields a
                   -> O.Select fields
                   -> SelectDenotation a
denotationExplicit qr q =
  SelectArrDenotation (\conn r ->
    flip ($) r (\() -> O.runSelectExplicit qr conn q))

denotation :: O.Select Fields -> SelectDenotation Haskells
denotation = denotationExplicit fromFieldsFields

denotationArr' :: O.SelectArr Fields Fields
              -> SelectArrDenotation Haskells Haskells
denotationArr' q =
  SelectArrDenotation (\conn h ->
      let fs = pure (fieldsOfHaskells h)
      in O.runSelectExplicit fromFieldsFields conn (q <<< fs))

denotationArr :: O.SelectArr FieldsTuple Fields
              -> SelectArrDenotation HaskellsTuple Haskells
denotationArr q =
  SelectArrDenotation (\conn h ->
      let fs = pure (O.toFields h)
      in O.runSelectExplicit fromFieldsFields conn (q <<< fs))

denotationMaybeFields :: O.Select (O.MaybeFields Fields)
                      -> SelectDenotation (Maybe Haskells)
denotationMaybeFields =
  denotationExplicit (O.fromFieldsMaybeFields fromFieldsFields)

unSelectDenotations :: Connection
                    -> SelectDenotation a
                    -> SelectDenotation b
                    -> ([a] -> [b] -> IO TQ.Property)
                    -> IO TQ.Property
unSelectDenotations conn one two k = unSelectArrDenotations conn one two () k

unSelectArrDenotations :: Connection
                       -> SelectArrDenotation i a
                       -> SelectArrDenotation i b
                       -> i
                       -> ([a] -> [b] -> IO TQ.Property)
                       -> IO TQ.Property
unSelectArrDenotations conn one two i k = do
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
              => Connection
              -> SelectDenotation a
              -> SelectDenotation a
              -> IO TQ.Property
compareNoSort conn one two =
  unSelectDenotations conn one two $ \one' two' -> do
  when (one' /= two')
       (putStrLn $ if List.sort one' == List.sort two'
                   then "[but they are equal sorted]"
                   else "AND THEY'RE NOT EVEN EQUAL SORTED!")

  return (one' === two')

compare :: (Show a, Ord a)
         => Connection
         -> SelectDenotation a
         -> SelectDenotation a
         -> IO TQ.Property
compare conn one two = unSelectDenotations conn one two $ \one' two' ->
  return (List.sort one' === List.sort two')

compareSortedBy :: (Show a, Ord a)
                => (a -> a -> Ord.Ordering)
                -> Connection
                -> SelectDenotation a
                -> SelectDenotation a
                -> IO TQ.Property
compareSortedBy o conn one two = unSelectDenotations conn one two $ \one' two' ->
  return ((List.sort one' === List.sort two')
          .&&. isSortedBy o one')

compareDenotation :: Connection
                  -> O.SelectArr Fields Fields
                  -> SelectArrDenotation Haskells Haskells
                  -> ArbitraryFields
                  -> IO TQ.Property
compareDenotation conn q d (ArbitraryFields f) =
  compare conn (denotation (q . pure f)) (d . denotation (pure f))

-- }

-- { The tests

fields :: Connection -> ArbitraryHaskells -> IO TQ.Property
fields conn (ArbitraryHaskells c) =
  compareNoSort conn (denotation (pure (fieldsOfHaskells c)))
                     (pure c)

compose :: Connection
        -> ArbitrarySelectArr
        -> ArbitrarySelectArr
        -> ArbitraryFields
        -> IO TQ.Property
compose conn (ArbitrarySelectArr q1) (ArbitrarySelectArr q2) = do
  compareDenotation conn (q1 . q2) (denotationArr' q1 . denotationArr' q2)

-- Would prefer to write 'compare conn (denotation id) id' but that
-- requires extending compare to compare SelectArrs.
identity :: Connection
         -> ArbitraryFields
         -> IO TQ.Property
identity conn = compareDenotation conn id id

fmap' :: Connection -> ArbitraryFunction -> ArbitrarySelect -> IO TQ.Property
fmap' conn (ArbitraryFunction f) (ArbitrarySelect q) =
  compareNoSort conn (denotation (fmap f q))
                     (fmap f (denotation q))

apply :: Connection
      -> ArbitrarySelectArr
      -> ArbitrarySelectArr
      -> ArbitraryFields
      -> IO TQ.Property
apply conn (ArbitrarySelectArr q1) (ArbitrarySelectArr q2) =
  compareDenotation conn (pair <$> q1 <*> q2)
                         (pair <$> denotationArr' q1 <*> denotationArr' q2)

  where pair x y = Choices [Right (pure x), Right (pure y)]


-- When combining arbitrary queries with the applicative product <*>
-- the limit of the denotation is not always the denotation of the
-- limit.  Without some ordering applied before the limit the returned
-- rows can vary.  If an ordering is applied beforehand we can check
-- the invariant that the returned rows always compare smaller than
-- the remainder under the applied ordering.
--
-- Strangely the same caveat doesn't apply to offset.
limit :: Connection
      -> ArbitraryPositiveInt
      -> ArbitrarySelect
      -> ArbitraryOrder
      -> IO TQ.Property
limit conn (ArbitraryPositiveInt l) (ArbitrarySelect q) o = do
  let q' = O.limit l (O.orderBy (arbitraryOrder o) q)

  unSelectDenotations conn (denotation q') (denotation q) $ \one' two' -> do
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

      return ((length one' === min l (length two'))
              .&&. condBool)

offset :: Connection -> ArbitraryPositiveInt -> ArbitrarySelect
       -> IO TQ.Property
offset conn (ArbitraryPositiveInt l) (ArbitrarySelect q) =
  compareNoSort conn (denotation (O.offset l q))
                     (onList (drop l) (denotation q))

order :: Connection -> ArbitraryOrder -> ArbitrarySelect -> IO TQ.Property
order conn o (ArbitrarySelect q) =
  compareSortedBy (arbitraryOrdering o)
                  conn
                  (denotation (O.orderBy (arbitraryOrder o) q))
                  (denotation q)

distinct :: Connection -> ArbitrarySelect -> IO TQ.Property
distinct conn (ArbitrarySelect q) =
  compare conn (denotation (O.distinctExplicit distinctFields q))
                (onList nub (denotation q))

-- When we added <*> to the arbitrary queries we started getting some
-- consequences to do with the order of the returned rows and so
-- restrict had to start being compared sorted.
restrict :: Connection -> ArbitraryFields -> IO TQ.Property
restrict conn =
  compareDenotation conn restrictFirstBool
                         (onList (>>= restrictFirstBoolListK) id)

values :: Connection -> ArbitraryHaskellsList -> IO TQ.Property
values conn (ArbitraryHaskellsList l) =
  compareNoSort conn
                (denotation (fmap fieldsList (O.valuesSafe (fmap O.toFields l))))
                (pureList (fmap fieldsList l))

-- We test values entries of length two in values, and values entries
-- of length zero here.  Ideally we would find some way to merge them.
valuesEmpty :: Connection -> [()] -> IO TQ.Property
valuesEmpty conn l =
  compareNoSort conn
                (denotationExplicit D.def (O.valuesSafe l))
                (pureList l)

aggregate :: Connection -> ArbitrarySelect -> IO TQ.Property
aggregate conn (ArbitrarySelect q) =
  compareNoSort conn (denotation (O.aggregate aggregateFields q))
                     (onList aggregateDenotation (denotation q))


label :: Connection -> String -> ArbitrarySelect -> IO TQ.Property
label conn comment (ArbitrarySelect q) =
  compareNoSort conn (denotation (O.label comment q))
                     (denotation q)

optional :: Connection -> ArbitrarySelect -> IO TQ.Property
optional conn (ArbitrarySelect q) =
  compare conn (denotationMaybeFields (OJ.optionalExplicit unpackFields q))
               (onList optionalDenotation (denotation q))

optionalRestrict :: Connection -> ArbitrarySelect -> IO TQ.Property
optionalRestrict conn (ArbitrarySelect q) =
  compare conn (denotationMaybeFields q1)
               (onList optionalRestrictDenotation (denotation q))
  where q1 = P.lmap (\() -> fst . firstBoolOrTrue (O.sqlBool True))
                    (O.optionalRestrictExplicit unpackFields q)

maybeFieldsToSelect :: Connection -> ArbitrarySelectMaybe -> IO TQ.Property
maybeFieldsToSelect conn (ArbitrarySelectMaybe q) =
  compare conn (denotation (O.maybeFieldsToSelect <<< q))
               (onList (Maybe.maybeToList =<<) (denotationMaybeFields q))

traverseMaybeFields :: Connection
                    -> ArbitrarySelectArr
                    -> ArbitrarySelectMaybe
                    -> IO TQ.Property
traverseMaybeFields conn (ArbitrarySelectArr q) (ArbitrarySelectMaybe qm) =
  compare conn
    (denotationMaybeFields (travMF q' . Arrow.arr (fmap listFields) . qm))
    (traverseDenotation (denotationArr q')
       ((fmap . fmap) listHaskells (denotationMaybeFields qm)))
  where u = unpackFields
        q' = q . Arrow.arr fieldsList
        travMF = O.traverseMaybeFieldsExplicit D.def u

lateral :: Connection
        -> ArbitraryKleisli
        -> ArbitrarySelect
        -> IO TQ.Property
lateral conn (ArbitraryKleisli f) (ArbitrarySelect q) =
  compare conn (lateralDenotation denotation_f . denotation_q)
               (denotationArr (O.lateral f') . denotation_q)
  where _ = f :: Fields -> O.Select Fields

        f' :: FieldsTuple -> O.Select Fields
        f' = f . Arrow.arr fieldsList

        denotation_q :: SelectDenotation HaskellsTuple
        denotation_q = fmap listHaskells (denotation q)

        denotation_f  :: HaskellsTuple -> SelectDenotation Haskells
        denotation_f = denotation . f' . O.toFields

{- TODO

  * Nullability
  * Operators (mathematical, logical, etc.)
  * Use traverseMaybeFields in generated queries

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
  let prop1 p = fmap          TQ.ioProperty (p conn)
      prop2 p = (fmap . fmap) TQ.ioProperty (p conn)
      prop3 p = (fmap . fmap . fmap) TQ.ioProperty (p conn)

      test1 :: (Show a, TQ.Arbitrary a, TQ.Testable prop)
               => (Connection -> a -> IO prop) -> IO ()
      test1 = t . prop1

      test2 :: (Show a1, Show a2, TQ.Arbitrary a1, TQ.Arbitrary a2,
                TQ.Testable prop)
               => (Connection -> a1 -> a2 -> IO prop) -> IO ()
      test2 = t . prop2

      test3 :: (Show a1, Show a2, Show a3,
                TQ.Arbitrary a1, TQ.Arbitrary a2, TQ.Arbitrary a3,
                TQ.Testable prop)
               => (Connection -> a1 -> a2 -> a3 -> IO prop) -> IO ()
      test3 = t . prop3

      t p = errorIfNotSuccess
        =<< TQ.quickCheckWithResult (TQ.stdArgs { TQ.maxSuccess = 1000 }) p

  test1 identity
  test3 compose
  test1 fields
  test2 fmap'
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

restrictFirstBoolList :: [Haskells] -> [Haskells]
restrictFirstBoolList hs = do
  h <- hs
  restrictFirstBoolListK h

restrictFirstBoolListK :: Haskells -> [Haskells]
restrictFirstBoolListK h = if fst (firstBoolOrTrue True h)
                           then [h]
                           else []

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
