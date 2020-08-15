> {-# LANGUAGE FlexibleContexts #-}
>
> module TutorialAdvanced where
>
> import           Prelude hiding (sum)
>
> import           Opaleye (Select, Field, Table, table, tableField,
>                           selectTable, SqlText, SqlInt4, Aggregator,
>                           aggregate)
> import qualified Opaleye.Aggregate as A
> import           Opaleye.Aggregate ()
>
> import qualified Opaleye.Sql as Sql
> import qualified Opaleye.Internal.Unpackspec as U
>
> import           Data.Profunctor.Product.Default (Default)
> import           Data.Profunctor (dimap)
> import           Data.Profunctor.Product ((***!), p2)


Combining Aggregators
=====================

Opaleye allows you to straightforwardly combine aggregators to create
new aggregators in a way that is inconvenient to do directly in
Postgres.

We can define an aggregator to calculate the range of a group, that is
the difference between its maximum and minimum.  Although we can write
this easily in SQL as `MAX(field) - MIN(field)`, Opaleye has the
advantage of treating `range` as a first-class value able to be passed
around between functions and manipulated at will.

> range :: Aggregator (Field SqlInt4) (Field SqlInt4)
> range = dimap (\x -> (x, x)) (uncurry (-)) (A.max ***! A.min)

We can test it on a person table which contains rows containing
people's names along with the age of their children.

> personTable :: Table (Field SqlText, Field SqlInt4)
>                      (Field SqlText, Field SqlInt4)
> personTable = table "personTable" (p2 ( tableField "name"
>                                       , tableField "child_age" ))

> rangeOfChildrensAges :: Select (Field SqlText, Field SqlInt4)
> rangeOfChildrensAges = aggregate (p2 (A.groupBy, range)) (selectTable personTable)


TutorialAdvanced> printSql rangeOfChildrensAges
SELECT result0_2 as result1,
       (result1_2) - (result2_2) as result2
FROM (SELECT *
      FROM (SELECT name0_1 as result0_2,
                   MAX(child_age1_1) as result1_2,
                   MIN(child_age1_1) as result2_2
            FROM (SELECT *
                  FROM (SELECT name as name0_1,
                               child_age as child_age1_1
                        FROM personTable as T1) as T1) as T1
            GROUP BY name0_1) as T1) as T1


Idealised SQL:

SELECT name,
       MAX(child_age) - MIN(child_age)
FROM personTable
GROUP BY name


Helper function
===============

> printSql :: Default U.Unpackspec a a => Select a -> IO ()
> printSql = putStrLn . maybe "Empty select" id . Sql.showSql
