> module TutorialManipulation where
>
> import           Prelude hiding (sum)
>
> import           Opaleye (Column, Table(Table),
>                           required, optional, (.==), (.<),
>                           arrangeDeleteSql, arrangeInsertSql,
>                           arrangeUpdateSql, arrangeInsertReturningSql,
>                           PGInt4, PGFloat8)
>
> import           Data.Profunctor.Product (p3)
> import           Data.Profunctor.Product.Default (Default, def)
> import qualified Opaleye.Internal.Unpackspec as U


Manipulation
============

Manipulation means changing the data in the database.  This means SQL
DELETE, INSERT and UPDATE.

To demonstrate manipulation in Opaleye we will need a table to perform
our manipulation on.  It will have three columns: an integer-valued
"id" column (assumed to be an auto-incrementing field) and two
double-valued required fields.  The `Table` type constructor has two
type arguments.  The first one is the type of writes to the table, and
the second is the type of reads from the table.  Notice that the "id"
column was defined as optional (for writes) so in the type of writes
it is wrapped in a Maybe.  That means we don't necessarily need to
specify it when writing to the table.  The database will automatically
fill in a value for us.

> table :: Table (Maybe (Column PGInt4), Column PGFloat8, Column PGFloat8)
>                (Column PGInt4, Column PGFloat8, Column PGFloat8)
> table = Table "tablename" (p3 (optional "id", required "x", required "y"))

To perform a delete we provide an expression from our read type to
`Column Bool`.  All rows for which the expression is true are deleted.

> delete :: String
> delete = arrangeDeleteSql table (\(_, x, y) -> x .< y)

ghci> putStrLn delete
DELETE FROM tablename
WHERE ((x) < (y))


To insert we provide a row with the write type.  Optional columns can
be omitted by providing `Nothing` instead.

> insertNothing :: String
> insertNothing = arrangeInsertSql table (Nothing, 2, 3)

ghci> putStrLn insertNothing
INSERT INTO tablename (x,
                       y)
VALUES (2.0,
        3.0)


If we really want to specify an optional column we can use `Just`.

> insertJust :: String
> insertJust = arrangeInsertSql table (Just 1, 2, 3)

ghci> putStrLn insertJust
INSERT INTO tablename (id,
                       x,
                       y)
VALUES (1,
        2.0,
        3.0)


An update takes an update function from the read type to the write
type, and a condition given by a function from the read type to
`Column Bool`.  All rows that satisfy the condition are updated
according to the update function.

> update :: String
> update = arrangeUpdateSql table (\(_, x, y) -> (Nothing, x + y, x - y))
>                                 (\(id_, _, _) -> id_ .== 5)

ghci> putStrLn update
UPDATE tablename
SET x = (x) + (y),
    y = (x) - (y)
WHERE ((id) = 5)


Sometimes when we insert a row with an automatically generated field
we want the database to return the new field value to us so we can use
it in future queries.  SQL supports that via INSERT RETURNING and
Opaleye supports it also.

> insertReturning :: String
> insertReturning = arrangeInsertReturningSql def' table (Nothing, 4, 5)
>                                             (\(id_, _, _) -> id_)
>                   -- TODO: vv This is too messy
>                   where def' :: U.Unpackspec (Column a) (Column a)
>                         def' = def

ghci> putStrLn insertReturning
INSERT INTO tablename (x,
                       y)
VALUES (4.0,
        5.0)
RETURNING id


Running the queries
===================

This tutorial has only shown you how to generate the SQL string for
manipulation queries.  In practice you actually want to run them!  To
run them you should use `runInsert` instead of `arrangeInsertSql`,
`runDelete` instead of `arrangeDeleteSql`, etc..


Comments
========

Opaleye does not currently support inserting more than one row at
once, or SELECT-valued INSERT or UPDATE.
