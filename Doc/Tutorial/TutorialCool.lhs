> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> {-# OPTIONS_GHC -Wno-missing-signatures #-}
>
> module TutorialCool where
>
> import           Opaleye ((.===), (.++), (.<))
> import qualified Opaleye as O
> import qualified Opaleye.Join as J
> import qualified Database.PostgreSQL.Simple as PGS
> import Database.Postgres.Temp
> import Data.Profunctor as P
> import Data.Profunctor.Product.Default
> import Data.Profunctor.Product.TH
>
>
> example1 = pure (O.toFieldsI "Hello")
>
> example2 = pure (O.toFieldsI ("Hello", 1 :: Int))
>
> rows = [ (1 :: Int, "Hello")
>        , (2, "Goodbye")
>        , (3, "Bonjour")
>        ]
>
> rows2 = [ (1 :: Int, "world")
>         , (2, "cruel world") ]
>
> example3 = O.valuesSafe (map O.toFieldsI rows)
>
> example4 = do
>   (i1, s1) <- example3
>   (i2, s2) <- O.valuesSafe (map O.toFieldsI rows2)
>   O.viaLateral O.restrict (i1 .=== i2)
>   pure (s1 .++ O.sqlString " " .++ s2)
>
> example5__1 = J.optional (O.valuesSafe (map O.toFieldsI [ "Hello", "world" ]))
>
> example5__2 :: O.Select (O.MaybeFields ())
> example5__2 = J.optional (O.valuesSafe (map O.toFieldsI []))
>
> example5 = do
>   (i1, s1) <- example3
>   s2 <- J.optional $ do
>     (i2, s2) <- O.valuesSafe (map O.toFieldsI rows2)
>     O.viaLateral O.restrict (i1 .=== i2)
>     pure s2
>   pure (s1, s2)
>
> example5_1 = pure (O.justFields (O.sqlString "Hello"))
>
> example5_2 :: O.Select (O.MaybeFields ())
> example5_2 = pure O.nothingFields
>
> example6 = do
>   (i1, s1) <- example3
>   s2 <- J.optional $ do
>     (i2, s2) <- O.valuesSafe (map O.toFieldsI rows2)
>     O.viaLateral O.restrict (i1 .=== i2)
>     pure s2
>   pure (s1
>         .++ O.sqlString " "
>         .++ O.fromMaybeFields (O.sqlString "et c'est tout" ) s2)
>
> example7 = O.viaLateral O.restrict (O.sqlBool True)
>
> example8 = O.viaLateral O.restrict (O.sqlBool False)
>
> data Rec a b = Rec a b deriving Show
>
> $(makeAdaptorAndInstanceInferrable "pRec" ''Rec)
>
> example9 = pure (O.toFieldsI (Rec "Hello" "world"))
>
> example10 = O.union example4 example6
>
> example11 = do
>   k <- O.distinct $ do
>       (k, _) <- as_bs
>       pure k
>
>   let foo2 :: O.Select (O.Field O.SqlInt4)
>       foo2 = fmap (O.unsafeCast "int4") $ O.aggregate O.sum $ do
>         (k', i) <- as_bs
>         O.viaLateral O.restrict (k .=== k')
>         pure i
>
>   r <- foo2
>   pure (k, r)
>
> example11_nested = do
>   k <- O.distinct $ do
>       (k, _) <- as_bs
>       pure k
>
>   let foo2 :: O.Select (O.Field O.SqlFloat8)
>       foo2 = do
>         (k', i) <- as_bs
>         O.viaLateral O.restrict (k .=== k')
>         pure i
>
>   pure (k, foo2)
>
> unnest :: O.Select (a, O.Select (O.Field b))
>        -> O.Select (a, (O.Field (O.SqlArray b)))
> unnest q = do
>   (k, inner) <- q
>   array <- O.aggregate O.arrayAgg inner
>   pure (k, array)
>
> example11_2 = unnest example11_nested
>
> as_bs = O.valuesSafe (map O.toFieldsI [ ("a", 1 :: Double)
>                                       , ("a", 2)
>                                       , ("b", 10)
>                                       , ("b", 20)
>                                       , ("c", 100)
>                                       , ("c", 200)
>                                       ])
>
> groupBy_ :: (Default O.EqPP k k,
>              Default O.Distinctspec k k)
>          => (a -> k) -> O.Select a -> O.Select (k, O.Select a)
> groupBy_ f s = do
>   k <- O.distinct $ do
>       t <- s
>       pure (f t)
>
>   let group = do
>         t <- s
>         O.viaLateral O.restrict (k .=== f t)
>         pure t
>
>   pure (k, group)
>
> example11_1 = do
>   (k, s) <- groupBy_ fst as_bs
>   sum' <- O.laterally (O.aggregate O.sum) (fmap snd s)
>   avg' <- O.laterally (O.aggregate O.avg) (fmap snd s)
>   str' <- O.laterally (O.aggregate (O.stringAgg (O.sqlString ","))) (fmap fst s)
>   pure (k, sum', avg', str')
>
> author_table = O.values (map O.toFieldsI (
>   [ (0 :: Int, "Simon", "Peyton Jones")
>   , (1, "Simon", "Marlow")
>   , (2, "Richard", "Eisenberg")
>   , (3, "Edward", "Kmett")
>   , (4, "Andrey", "Mokhov")
>   , (5, "Joachim", "Breitner")
>   , (6, "Stephanie", "Weirich")
>   , (7, "Judith", "Borghouts")
>   , (8, "Matt", "McCutchen")
>   , (9, "Andy", "Gordon")
>   , (10, "Advait", "Sarkar")
>   , (11, "Sigbjorn", "Finne")
>   ]))
>
> paper_table = O.values (map O.toFieldsI (
>   [ (0 :: Int, "Making a fast curry", 2004 :: Int)
>   , (1, "Levity polymorphism", 2017)
>   , (2, "Desugaring Haskell's do-notation Into Applicative Operations", 2016)
>   , (3, "Safe zero-cost coercions for Haskell", 2016)
>   , (4, "Elastic Sheet-Defined Functions", 2020)
>   , (5, "Concurrent Haskell", 1996)
>   ]))
>
> paper_author = O.values (map O.toFieldsI (
>   [ (0 :: Int, 0 :: Int)
>   , (0, 1)
>   , (1, 0)
>   , (1, 1)
>   , (2, 0)
>   , (2, 1)
>   , (2, 3)
>   , (2, 4)
>   , (3, 0)
>   , (3, 2)
>   , (3, 5)
>   , (3, 6)
>   , (4, 0)
>   , (4, 7)
>   , (4, 8)
>   , (4, 9)
>   , (4, 10)
>   , (5, 0)
>   , (5, 9)
>   , (5, 11)
>   ]))
>
> authorPaperCount = do
>   (authorId, authorFirstName, authorSurname) <- author_table
>   (paperCount, authorId') <-
>       O.aggregate ((,) <$> P.lmap fst O.count <*> P.lmap snd O.groupBy)
>                   paper_author
>
>   O.viaLateral O.restrict (authorId .=== authorId')
>
>   pure (authorFirstName .++ O.sqlString " " .++ authorSurname
>        .++ O.sqlString " has "
>        .++ pluralise paperCount (O.sqlString "paper")
>        .++ O.sqlString " in the database")
>
> sameNamePapers = do
>   (paperId, paperName, _) <- paper_table
>   (author1Id, author1FirstName, author1Surname) <- author_table
>   (author2Id, author2FirstName, author2Surname) <- author_table
>   (paperId1', author1Id') <- paper_author
>   (paperId2', author2Id') <- paper_author
>
>   O.viaLateral O.restrict (paperId .=== paperId1')
>   O.viaLateral O.restrict (paperId .=== paperId2')
>   O.viaLateral O.restrict (author1Id' .=== author1Id)
>   O.viaLateral O.restrict (author2Id' .=== author2Id)
>
>   O.viaLateral O.restrict (author1Id .< author2Id)
>   O.viaLateral O.restrict (author1FirstName .=== author2FirstName)
>
>   let jointFirstName = author1FirstName
>
>   return (jointFirstName .++ O.sqlString "s "
>           .++ author1Surname
>           .++ O.sqlString " and "
>           .++ author2Surname
>           .++ O.sqlString " collaborated on "
>           .++ paperName)
>
> paperGaps = do
>   (paperEarlierId, paperEarlierName, paperEarlierYear) <- paper_table
>   (paperLaterId,   paperLaterName,   paperLaterYear)   <- paper_table
>
>   (author1Id, author1FirstName, author1Surname) <- author_table
>   (author2Id, author2FirstName, author2Surname) <- author_table
>
>   (paperEarlierId1, authorEarlierId1) <- paper_author
>   (paperEarlierId2, authorEarlierId2) <- paper_author
>   (paperLaterId1,   authorLaterId1) <- paper_author
>   (paperLaterId2,   authorLaterId2) <- paper_author
>
>   O.viaLateral O.restrict (paperEarlierId .=== paperEarlierId1)
>   O.viaLateral O.restrict (paperEarlierId .=== paperEarlierId2)
>   O.viaLateral O.restrict (paperLaterId .=== paperLaterId1)
>   O.viaLateral O.restrict (paperLaterId .=== paperLaterId2)
>
>   O.viaLateral O.restrict (author1Id .=== authorEarlierId1)
>   O.viaLateral O.restrict (author1Id .=== authorLaterId1)
>
>   O.viaLateral O.restrict (author2Id .=== authorEarlierId2)
>   O.viaLateral O.restrict (author2Id .=== authorLaterId2)
>
>   O.viaLateral O.restrict (author1Id .< author2Id)
>   O.viaLateral O.restrict (paperEarlierYear .< paperLaterYear)
>
>   pure (author1FirstName .++ O.sqlString " " .++ author1Surname
>         .++ O.sqlString " and "
>         .++ author2FirstName .++ O.sqlString " " .++ author2Surname
>         .++ O.sqlString " collaborated on "
>         .++ paperEarlierName .++ O.sqlString " and " .++ paperLaterName
>         .++ O.sqlString " separated by "
>         .++ years (paperLaterYear - paperEarlierYear))
>
>   where years n = pluralise n (O.sqlString "year")
>
> pluralise n noun =
>   O.unsafeCast "text" n .++ O.sqlString " " .++ noun
>   .++ (O.ifThenElse (n .=== 1)
>                     (O.sqlString "")
>                     (O.sqlString "s"))
>
> authorsOf paperId = do
>   author@(authorId, _, _) <- author_table
>   (paperId', authorId') <- paper_author
>   O.viaLateral O.restrict (authorId .=== authorId')
>   O.viaLateral O.restrict (paperId .=== paperId')
>
>   pure author
>
> collaborators = do
>   paper@(paperId, _, _) <- paper_table
>   author1@(author1Id, _, _) <- authorsOf paperId
>   author2@(author2Id, _, _) <- authorsOf paperId
>
>   O.viaLateral O.restrict (author1Id .< author2Id)
>
>   pure (paper, author1, author2)
>
> sameNamePapersSimpler = do
>   ((_, paperName, _),
>    (_, author1FirstName, author1Surname),
>    (_, author2FirstName, author2Surname)) <- collaborators
>
>   O.viaLateral O.restrict (author1FirstName .=== author2FirstName)
>
>   let jointFirstName = author1FirstName
>
>   return (jointFirstName .++ O.sqlString "s "
>           .++ author1Surname
>           .++ O.sqlString " and "
>           .++ author2Surname
>           .++ O.sqlString " collaborated on "
>           .++ paperName)
>
> paperGapsSimpler = do
>   ((_, paperEarlierName, paperEarlierYear),
>    (author1Id, author1FirstName, author1Surname),
>    (author2Id, author2FirstName, author2Surname)) <- collaborators
>
>   ((_, paperLaterName, paperLaterYear),
>    (author1Id', _, _),
>    (author2Id', _, _)) <- collaborators
>
>   O.viaLateral O.restrict (author1Id .=== author1Id')
>   O.viaLateral O.restrict (author2Id .=== author2Id')
>   O.viaLateral O.restrict (paperEarlierYear .< paperLaterYear)
>
>   pure (author1FirstName .++ O.sqlString " " .++ author1Surname
>         .++ O.sqlString " and "
>         .++ author2FirstName .++ O.sqlString " " .++ author2Surname
>         .++ O.sqlString " collaborated on "
>         .++ paperEarlierName .++ O.sqlString " and " .++ paperLaterName
>         .++ O.sqlString " separated by "
>         .++ years (paperLaterYear - paperEarlierYear))
>
>   where years n = pluralise n (O.sqlString "year")
>
> example12 = do
>   r <- O.valuesSafe (map O.toFieldsI rows2)
>   s <- O.viaLateral (O.leftJoinA (pure (1 :: O.Field O.SqlInt4, O.sqlString "Hello")))
>                     (const (O.sqlBool True))
>   pure (r, s)
>
>
> example13 =
>   let (a1, s1) = O.toFieldsI (O.sqlArray O.toFieldsI [1 :: Int,2,3], "Hello")
>       (a2, s2) = O.toFieldsI (O.sqlArray O.toFieldsI [4 :: Int,5,6], " world")
>   in pure (a1 `O.arrayAppend` a2, s1 O..++ s2)
>
> --run
> -- :: (Default (O.Wrap O.FromFields) fields a, Show a)
> -- => O.Select fields -> IO (Either StartError [a])
> run s =
>   do { putStr "Starting DB..."
>      ; r <- with (\db -> do { putStrLn "done."
>                             ; let { connectString = toConnectionString db }
>                             ; conn <- PGS.connectPostgreSQL connectString
>                             ; ts <- O.runSelectI conn s; pure ts })
>      ; case r of
>        { Left  e  -> fail ("Database error: " ++ show e)
>        ; Right ts -> pure ts
>        }
>      }
