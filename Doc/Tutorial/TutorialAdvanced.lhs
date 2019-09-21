> {-# LANGUAGE FlexibleContexts #-}
>
> module TutorialAdvanced where
>
> import           Prelude hiding (sum)
>
> import           Opaleye (Select, Field, Table, table, tableField,
>                           selectTable, SqlText, SqlInt4, Aggregator,
>                           aggregate)
> import qualified Opaleye as O
> import qualified Opaleye.Aggregate as A
> import           Opaleye.Aggregate ()
>
> import qualified Opaleye.Sql as Sql
> import           Opaleye.Internal.Dynamic (stringUnpackspec, SqlDynamic)
> import qualified Opaleye.Internal.Unpackspec as U
> import qualified Opaleye.Internal.Table as T
>
> import           Data.Profunctor.Product.Default (Default)
> import qualified Data.Profunctor.Product.Default as D
> import           Data.Profunctor (dimap)
> import           Data.Profunctor.Product ((***!), p2)
> import qualified Data.Profunctor.Product as PP
> import           Data.Void as V


Combining Aggregators
=====================

Opaleye allows you to straightforwardly combine aggregators to create
new aggregators in a way that is inconvenient to do directly in
Postgres.

We can define an aggregator to calculate the range of a group, that is
the difference between its maximum and minimum.  Although we can write
this easily in SQL as `MAX(column) - MIN(column)`, Opaleye has the
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


Dynamic types
=============

Opaleye can select from tables even if you only know the names of the
 fields at run time.

> dynamicTable :: Table V.Void [(String, Field SqlDynamic)]
> dynamicTable =
>     table "dynamicTable"
>           (T.fromDynamicTableFields
>              ((traverse._2) T.dynamic
>                 [ ("foo", "foo"), ("bar", "bar") ]))
>   where _2 f (a, b) = fmap (\b' -> (a, b')) (f b)

> dynamicTableExample :: IO ()
> dynamicTableExample = printSqlExplicit listOfPairs foo
>   where foo = O.selectTableExplicit listOfPairs dynamicTable
>         matchType :: p a a -> p a a
>         matchType = id
>         listOfPairs :: U.Unpackspec [(String, O.Column SqlDynamic)]
>                                     [(String, O.Column SqlDynamic)]
>         listOfPairs = matchType (PP.list (p2 (stringUnpackspec, D.def)))

Helper function
===============

> printSql :: Default U.Unpackspec a a => Select a -> IO ()
> printSql = printSqlExplicit D.def

> printSqlExplicit :: U.Unpackspec a a -> Select a -> IO ()
> printSqlExplicit u =
>   putStrLn . maybe "Empty query" id . Sql.showSqlForPostgresExplicit u
