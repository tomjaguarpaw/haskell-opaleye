> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TemplateHaskell #-}
>
> module TutorialBasic where
>
> import           Prelude hiding (sum)
>
> import           Opaleye.QueryArr (Query, QueryArr)
> import           Opaleye.Column (Column, Nullable, matchNullable, isNull)
> import           Opaleye.Table (Table(Table), required, queryTable)
> import           Opaleye.Operators (restrict, (.==), (.<=), (.&&), (.<),
>                                     (.++), ifThenElse)
> import           Opaleye.PGTypes (pgString)
> import           Opaleye.Aggregate (aggregate, groupBy, count, avg, sum)
> import           Opaleye.Join (leftJoin)
> import           Opaleye.RunQuery (runQuery)
>
> import qualified Opaleye.Sql as Sql
> import qualified Opaleye.Internal.Unpackspec as U
>
> import           Data.Profunctor.Product (p3, p5)
> import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
> import           Data.Time.Calendar (Day)
>
> import           Data.Profunctor.Product.Default (Default, def)
> import           Data.Profunctor (dimap)
> import           Data.Profunctor.Product (ProductProfunctor, p2)
>
> import           Control.Arrow (returnA, (<<<))
>
> import qualified Database.PostgreSQL.Simple as PGS
>
> import           GHC.Int (Int64)

Introduction
============

In this example file I'll give you a brief introduction to the Opaleye
relational query EDSL.  I'll show you how to define tables in Opaleye;
use them to generate selects, joins and filters; use the API of
Opaleye to make your queries more composable; and finally run the
queries on Postgres.

Opaleye uses HaskellDB's SQL generator.  You should make sure you use
the version suggested in README.md.

Schema
======

A table is defined with the `Table` constructor.  The syntax is
simple.  You specify the types of the columns, the name of the table
and the names of the columns in the underlying database, and whether
the columns are required or optional.

(Note: This simple syntax is supported by an extra combinator that
describes the shape of the container that you are storing the columns
in.  In the first example we are using a tuple of size 3 and the
combinator is called `p3`.  We'll see examples of others later.)

The `Table` type constructor has two arguments.  The first one tells
us what columns we can write to the table and the second what columns
we can read from the table.  In this document we will always make all
columns required, so the write and read types will be the same.  All
`Table` types will have the same type argument repeated twice.  In the
advanced tutorial you can see an example of when they might differ.

> personTable :: Table (Column String, Column Int, Column String)
>                      (Column String, Column Int, Column String)
> personTable = Table "personTable" (p3 ( required "name"
>                                       , required "age"
>                                       , required "address" ))

To query a table we use `queryTable`.

(Here and in a few other places in Opaleye there is some typeclass
magic going on behind the scenes.  However, you never *have* to use
typeclasses.  All the magic that typeclasses do is also available by
explicitly passing in the "typeclass dictionary".  For this example
file we will always use the typeclass versions because they are
simpler to read and the typeclass magic is essentially invisible.)

> personQuery :: Query (Column String, Column Int, Column String)
> personQuery = queryTable personTable

A `Query` corresponds to an SQL SELECT that we can run.  Here is the
SQL generated for `personQuery`.

ghci> printSql personQuery
SELECT name0_1 as result1,
       age1_1 as result2,
       address2_1 as result3
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1) as T1

This SQL is functionally equivalent to the following "idealized" SQL.
Eventually Opaleye should generate SQL closer to the "idealized"
version, but that is an ongoing project.

SELECT name,
       age
       address
FROM personTable

(`printSQL` is just a convenient utility function for the purposes of
this example file.  See below for its definition.)


Record types
------------

Opaleye can use user defined types such as record types in queries.

It will save you a lot of headaches if you define your data types to
be polymorphic in all their fields.  If you want to use concrete types
in particular places, as you almost always will, you can use type
synonyms.  For example:

> data Birthday' a b = Birthday { bdName :: a, bdDay :: b }
> type Birthday = Birthday' String Day
> type BirthdayColumn = Birthday' (Column String) (Column Day)

To get user defined types to work with the typeclass magic they must
have instances defined for them.  The instances are derivable with
Template Haskell.

> $(makeAdaptorAndInstance "pBirthday" ''Birthday')

Then we can use 'Table' to make a table on our record type in exactly
the same way as before.

> birthdayTable :: Table BirthdayColumn BirthdayColumn
> birthdayTable = Table "birthdayTable"
>                        (pBirthday Birthday { bdName = required "name"
>                                            , bdDay  = required "birthday" })
>
> birthdayQuery :: Query BirthdayColumn
> birthdayQuery = queryTable birthdayTable

ghci> printSql birthdayQuery
SELECT name0_1 as result1,
       birthday1_1 as result2
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   birthday as birthday1_1
            FROM birthdayTable as T1) as T1) as T1

Idealized SQL:

SELECT name,
       birthday
FROM birthdayTable


Projection
==========

"Projection" means discarding some of the columns of our query, for
example we might want to discard the "address" column of our
`personQuery`.

Projection gives us our first example of using "arrow notation" to
write Opaleye queries.  Arrow notation is essentially a restricted
version of "do notation".  Arrow notation allows you to write arrow
computations, and do notation allows you to write monadic
computations.

Here we run the `personQuery` passing in () to signify "zero
arguments".  We pattern match on the results and return only the
columns we are interested in.

> nameAge :: Query (Column String, Column Int)
> nameAge = proc () -> do
>   (name, age, _) <- personQuery -< ()
>   returnA -< (name, age)

ghci> printSql nameAge
SELECT name0_1 as result1,
       age1_1 as result2
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1) as T1

Idealized SQL:

SELECT name,
       age
FROM personTable

Product
=======

"Product" means taking the Cartesian product of two queries.  This is
simple in arrow notation.  Here we take the product of `personQuery`
and `birthdayQuery`.

> personBirthdayProduct ::
>   Query ((Column String, Column Int, Column String), BirthdayColumn)
> personBirthdayProduct = proc () -> do
>   personRow   <- personQuery -< ()
>   birthdayRow <- birthdayQuery -< ()
>
>   returnA -< (personRow, birthdayRow)

ghci> printSql personBirthdayProduct
SELECT name0_1 as result1,
       age1_1 as result2,
       address2_1 as result3,
       name0_2 as result4,
       birthday1_2 as result5
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1,
           (SELECT name as name0_2,
                   birthday as birthday1_2
            FROM birthdayTable as T1) as T2) as T1

Idealized SQL:

SELECT name0,
       age0,
       address0,
       name1,
       birthday1
FROM (SELECT name as name0,
             age as age0,
             address as address0
      FROM personTable as T1),
     (SELECT name as name1,
             birthday as birthday1
      FROM birthdayTable as T1)


Restriction
===========

"Restriction" means restricting the rows of the result of a query to
only those where some condition holds.

We can restrict `personQuery` to the rows where the person is up to 18
years old.

> youngPeople :: Query (Column String, Column Int, Column String)
> youngPeople = proc () -> do
>   row@(_, age, _) <- personQuery -< ()
>   restrict -< age .<= 18
>
>   returnA -< row

ghci> printSql youngPeople
SELECT name0_1 as result1,
       age1_1 as result2,
       address2_1 as result3
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1
      WHERE ((age1_1) <= 18)) as T1

Idealized SQL:

SELECT name,
       age,
       address
FROM personTable
WHERE age <= 18


We can use a variety of operators to form more complex restriction
conditions.

> twentiesAtAddress :: Query (Column String, Column Int, Column String)
> twentiesAtAddress = proc () -> do
>   row@(_, age, address) <- personQuery -< ()
>
>   restrict -< (20 .<= age) .&& (age .< 30)
>   restrict -< address .== pgString "1 My Street, My Town"
>
>   returnA -< row

SELECT name0_1 as result1,
       age1_1 as result2,
       address2_1 as result3
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1
      WHERE ((address2_1) = '1 My Street, My Town') AND ((20 <= (age1_1))
             AND ((age1_1) < 30))) as T1

Idealized SQL:

SELECT name,
       age,
       address
FROM personTable
WHERE address = '1 My Street, My Town'
AND   20 <= age
AND   age < 30


Inner join
----------

A Product followed by a restriction is sometimes called a "join" or
"inner join" in SQL terminology.  The following query is an example of
such.

> personAndBirthday ::
>   Query (Column String, Column Int, Column String, Column Day)
> personAndBirthday = proc () -> do
>   (name, age, address) <- personQuery -< ()
>   birthday             <- birthdayQuery -< ()
>
>   restrict -< name .== bdName birthday
>
>   returnA -< (name, age, address, bdDay birthday)


ghci> printSql personAndBirthday
SELECT name0_1 as result1,
       age1_1 as result2,
       address2_1 as result3,
       birthday1_2 as result4
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1,
           (SELECT name as name0_2,
                   birthday as birthday1_2
            FROM birthdayTable as T1) as T2
      WHERE ((name0_1) = (name0_2))) as T1

Idealized SQL:

SELECT name0,
       age0,
       address0,
       name1,
       birthday1
FROM (SELECT name as name0,
             age as age0,
             address as address0
      FROM personTable as T1),
     (SELECT name as name1,
             birthday as birthday1
      FROM birthdayTable as T1)
WHERE name0 == name1


Nullability
===========

NULLs in SQL have been the source of a lot of complaints, but as
Haskell programmers we know that there is nothing wrong with
nullability as long is it is reflected in the type system.  Nullable
columns are indicated with the `Nullable` type constructor.

For example, suppose we have an employee table which records the name
of each employee and the name of their boss.  If their boss is
recorded as NULL then that means they have no boss!

> employeeTable :: Table (Column String, Column (Nullable String))
>                        (Column String, Column (Nullable String))
> employeeTable = Table "employeeTable" (p2 ( required "name"
>                                           , required "boss" ))

We can write a query that returns as string indicating for each
employee whether they have a boss.

> hasBoss :: Query (Column String)
> hasBoss = proc () -> do
>   (name, nullableBoss) <- queryTable employeeTable -< ()
>
>   let aOrNo = ifThenElse (isNull nullableBoss) (pgString "no") (pgString "a")
>
>   returnA -< name .++ pgString " has " .++ aOrNo .++ pgString " boss"

SELECT (((name0_1) || ' has ')
       || (CASE WHEN boss1_1 IS NULL THEN 'no' ELSE 'a' END))
       || ' boss' as result1
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   boss as boss1_1
            FROM employeeTable as T1) as T1) as T1

Idealized SQL:

SELECT name || ' has '
            || CASE WHEN boss IS NULL THEN 'no' ELSE 'a' END || ' boss'
FROM employeeTable

But we can do much more than just check for NULL of course.  We can
write a query arrow to produce a string describing each employee's
status along with the name of their boss, if any.  The combinator
`matchNullable` checks whether `nullableBoss` is NULL.  If so it
returns its first argument.  If not it passes the non-NULL value to
the function that is the second argument.

> bossQuery :: QueryArr (Column String, Column (Nullable String)) (Column String)
> bossQuery = proc (name, nullableBoss) -> do
>   returnA -< matchNullable (name .++ pgString " has no boss")
>                            (\boss -> pgString "The boss of " .++ name
>                                      .++ pgString " is " .++ boss)
>                            nullableBoss

Note that `matchNullable` corresponds to Haskell's

    maybe :: b -> (a -> b) -> Maybe a -> b

and in pure Haskell the same computation could be expressed as

> bossHaskell :: (String, Maybe String) -> String
> bossHaskell (name, nullableBoss) = maybe (name ++ " has no boss")
>                                          (\boss -> "The boss of " ++ name
>                                                    ++ " is " ++ boss)
>                                          nullableBoss

Then we get the following SQL.

ghci> printSql (bossQuery <<< queryTable employeeTable)
SELECT CASE WHEN boss1_1 IS NULL THEN (name0_1) || ' has no boss'
     ELSE (('The boss of ' || (name0_1)) || ' is ') || (boss1_1) END as result1
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   boss as boss1_1
            FROM employeeTable as T1) as T1) as T1

Idealized SQL:

SELECT CASE WHEN boss IS NULL
            THEN name0_1 || ' has no boss'
            ELSE 'The boss of ' || name || ' is ' || boss
            END
FROM employeeTable


Composability
=============

Rewriting `twentiesAtAddress` will allow us to get our first glimpse
of the enormous composability that Opaleye offers.

We can factor out some parts of the 'twentiesAtAddress' query.  For
example we can pull out the restriction to one's age being "in the
twenties" and the restriction to the one's address being "1 My Street,
My Town".

The types are of the form `QueryArr a ()`.  This means that they read
columns of type `a` but do not return any columns.  (Note: `Query` is
just a synonym for `QueryArr ()` which means that it is a `QueryArr`
that does not read any columns.)

> restrictIsTwenties :: QueryArr (Column Int) ()
> restrictIsTwenties = proc age -> do
>   restrict -< (20 .<= age) .&& (age .< 30)
>
> restrictAddressIs1MyStreet :: QueryArr (Column String) ()
> restrictAddressIs1MyStreet = proc address -> do
>   restrict -< address .== pgString "1 My Street, My Town"

We can't generate "the SQL of" these combinators.  They are not
`Query`s so they don't have any SQL!  (This corresponds to the
observation that in Haskell typically values can be "shown", but
functions cannot be "shown".) Instead we use them to reimplement
`twentiesAtAddress` in a more neatly-factored way.

> twentiesAtAddress' :: Query (Column String, Column Int, Column String)
> twentiesAtAddress' = proc () -> do
>   row@(_, age, address) <- personQuery -< ()
>
>   restrictIsTwenties -< age
>   restrictAddressIs1MyStreet -< address
>
>   returnA -< row

The SQL generated is exactly the same as before

ghci> printSql twentiesAtAddress'
SELECT name0_1 as result1,
       age1_1 as result2,
       address2_1 as result3
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1
      WHERE ((address2_1) = '1 My Street, My Town') AND ((20 <= (age1_1))
             AND ((age1_1) < 30))) as T1


Composability of joins
----------------------

We can perform a similar transformation for `personAndBirthday` by
pulling out a `QueryArr` which perform the mapping of a person's name
to their date of birth by looking up in `birthdayQuery`.

> birthdayOfPerson :: QueryArr (Column String) (Column Day)
> birthdayOfPerson = proc name -> do
>   birthday <- birthdayQuery -< ()
>
>   restrict -< name .== bdName birthday
>
>   returnA -< bdDay birthday

We can then reimplement `personAndBirthday` as follows

> personAndBirthday' ::
>   Query (Column String, Column Int, Column String, Column Day)
> personAndBirthday' = proc () -> do
>   (name, age, address) <- personQuery -< ()
>   birthday <- birthdayOfPerson -< name
>
>   returnA -< (name, age, address, birthday)

and it yields the same SQL as before.

ghci> printSql personAndBirthday'
SELECT name0_1 as result1,
       age1_1 as result2,
       address2_1 as result3,
       birthday1_2 as result4
FROM (SELECT *
      FROM (SELECT name as name0_1,
                   age as age1_1,
                   address as address2_1
            FROM personTable as T1) as T1,
           (SELECT name as name0_2,
                   birthday as birthday1_2
            FROM birthdayTable as T1) as T2
      WHERE ((name0_1) = (name0_2))) as T1



Aggregation
===========

Type safe aggregation is the jewel in the crown of Opaleye.  Even SQL
generating APIs which are otherwise type safe often fall down when it
comes to aggregation.  If you want to find holes in the type system of
an SQL generating language, aggregation is the best place to look!  By
contrast, Opaleye aggregations always generate meaningful SQL.

By way of example, suppose we have a widget table which contains the
style, color, location, quantity and radius of widgets.  We can model
this information with the following datatype.

> data Widget a b c d e = Widget { style    :: a
>                                , color    :: b
>                                , location :: c
>                                , quantity :: d
>                                , radius   :: e }
>
> $(makeAdaptorAndInstance "pWidget" ''Widget)

For the purposes of this example the style, color and location will be
strings, but in practice they might have been a different data type.

> widgetTable :: Table (Widget (Column String) (Column String) (Column String)
>                              (Column Int) (Column Double))
>                      (Widget (Column String) (Column String) (Column String)
>                              (Column Int) (Column Double))
> widgetTable = Table "widgetTable"
>                      (pWidget Widget { style    = required "style"
>                                      , color    = required "color"
>                                      , location = required "location"
>                                      , quantity = required "quantity"
>                                      , radius   = required "radius" })


Say we want to group by the style and color of widgets, calculating
how many (possibly duplicated) locations there are, the total number
of such widgets and their average radius.  `aggregateWidgets` shows us
how to do this.

> aggregateWidgets :: Query (Widget (Column String) (Column String) (Column Int64)
>                                   (Column Int) (Column Double))
> aggregateWidgets = aggregate (pWidget (Widget { style    = groupBy
>                                               , color    = groupBy
>                                               , location = count
>                                               , quantity = sum
>                                               , radius   = avg }))
>                              (queryTable widgetTable)

The generated SQL is

ghci> printSql aggregateWidgets
SELECT result0_2 as result1,
       result1_2 as result2,
       result2_2 as result3,
       result3_2 as result4,
       result4_2 as result5
FROM (SELECT *
      FROM (SELECT style0_1 as result0_2,
                   color1_1 as result1_2,
                   COUNT(location2_1) as result2_2,
                   SUM(quantity3_1) as result3_2,
                   AVG(radius4_1) as result4_2
            FROM (SELECT *
                  FROM (SELECT style as style0_1,
                               color as color1_1,
                               location as location2_1,
                               quantity as quantity3_1,
                               radius as radius4_1
                        FROM widgetTable as T1) as T1) as T1
            GROUP BY style0_1,
                     color1_1) as T1) as T1

Idealized SQL:

SELECT style,
       color,
       COUNT(location),
       SUM(quantity),
       AVG(radius)
FROM widgetTable
GROUP BY style, color

Note: In `widgetTable` and `aggregateWidgets` we see more explicit
uses of our Template Haskell derived code.  We use the 'pWidget'
"adaptor" to specify how columns are aggregated.  Note that this is
yet another example of avoiding a headache by keeping your datatype
fully polymorphic, because the 'count' aggregator changes a 'Wire
String' into a 'Wire Int64'.

Outer join
==========

Opaleye supports left joins.  (Full outer joins and right joins are
left to be added as a simple starter project for a new Opaleye
contributer!)

Because left joins can change non-nullable columns into nullable
columns we have to make sure the type of the output supports
nullability.  We introduce the following type synonym for this
purpose, which is just a notational convenience.

> type ColumnNullableBirthday = Birthday' (Column (Nullable String))
>                                         (Column (Nullable Day))

A left join is expressed by specifying the two tables to join and the
join condition.

> personBirthdayLeftJoin :: Query ((Column String, Column Int, Column String),
>                                  ColumnNullableBirthday)
> personBirthdayLeftJoin = leftJoin personQuery birthdayQuery eqName
>     where eqName ((name, _, _), birthdayRow) = name .== bdName birthdayRow

The generated SQL is

ghci> printSql personBirthdayLeftJoin
SELECT result1_0_3 as result1,
       result1_1_3 as result2,
       result1_2_3 as result3,
       result2_0_3 as result4,
       result2_1_3 as result5
FROM (SELECT *
      FROM (SELECT name0_1 as result1_0_3,
                   age1_1 as result1_1_3,
                   address2_1 as result1_2_3,
                   name0_2 as result2_0_3,
                   birthday1_2 as result2_1_3
            FROM
            (SELECT *
             FROM (SELECT name as name0_1,
                          age as age1_1,
                          address as address2_1
                   FROM personTable as T1) as T1) as T1
            LEFT OUTER JOIN
            (SELECT *
             FROM (SELECT name as name0_2,
                          birthday as birthday1_2
                   FROM birthdayTable as T1) as T1) as T2
            ON
            (name0_1) = (name0_2)) as T1) as T1

Idealized SQL:

SELECT name0,
       age0,
       address0,
       name1,
       birthday1
FROM (SELECT name as name0,
             age as age0,
             address as address0
      FROM personTable) as T1
     LEFT OUTER JOIN
     (SELECT name as name1,
             birthday as birthday1
      FROM birthdayTable) as T1
ON name0 = name1



Running queries on Postgres
===========================


Opaleye provides simple facilities for running queries on Postgres.

For example we can run the 'twentiesAtAddress' query as below.
Note that this particular formulation uses typeclasses so please put
type signatures on everything in sight to minimize the number of
confusing error messages!

> runTwentiesQuery :: PGS.Connection -> IO [(String, Int, String)]
> runTwentiesQuery connection = runQuery connection twentiesAtAddress

Note that nullable columns are indicated with the Nullable type
constructor, and these are converted to Maybe when executed.  If we
have a table with a nullable column then Nullable columns turn into
Maybes.

> runEmployeesQuery :: PGS.Connection -> IO [(String, Maybe String)]
> runEmployeesQuery connection = runQuery connection (queryTable employeeTable)

Conclusion
==========

There ends the Opaleye introductions module.  Please send me your questions!

Utilities
=========

This is a little utility function to help with printing generated SQL.

> printSql :: Default U.Unpackspec a a => Query a -> IO ()
> printSql = putStrLn . Sql.showSqlForPostgres
