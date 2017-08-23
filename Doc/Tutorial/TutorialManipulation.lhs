> module TutorialManipulation where
>
> import           Prelude hiding (sum)
>
> import           Opaleye (Column, Table, table,
>                           tableColumn, (.==), (.<),
>                           arrangeDeleteSql, arrangeInsertManySql,
>                           arrangeUpdateSql, arrangeInsertManyReturningSql,
>                           PGInt4, PGFloat8)
>
> import           Data.Profunctor.Product (p4)
> import           Data.Profunctor.Product.Default (def)
> import qualified Opaleye.Internal.Unpackspec as U
> import qualified Opaleye.PGTypes as P
> import qualified Opaleye.Constant as C

Manipulation
============

Manipulation means changing the data in the database.  This means SQL
DELETE, INSERT and UPDATE.

To demonstrate manipulation in Opaleye we will need a table to perform
our manipulation on.  It will have three columns: an integer-valued
"id" column (assumed to be an auto-incrementing field) and two
double-valued required fields.  The `Table` type constructor has two
type arguments.  The first one is the type of writes to the table, and
the second is the type of reads from the table.  The "id"
column is defined as optional (for writes) because its write type is
`Maybe (Column PGInt4)`.  That means we don't necessarily need to
specify it when writing to the table.  The database will automatically
fill in a value for us.

> myTable :: Table
>     (Maybe (Column PGInt4), Column PGFloat8, Column PGFloat8, Column P.PGText)
>     (Column PGInt4, Column PGFloat8, Column PGFloat8, Column P.PGText)
> myTable = table "tablename" (p4 ( tableColumn "id"
>                                 , tableColumn "x"
>                                 , tableColumn "y"
>                                 , tableColumn "s" ))

To perform a delete we provide an expression from our read type to
`Column Bool`.  All rows for which the expression is true are deleted.

> delete :: String
> delete = arrangeDeleteSql myTable (\(_, x, y, _) -> x .< y)

ghci> putStrLn delete
DELETE FROM tablename
WHERE ((x) < (y))


To insert we provide a row with the write type.  Optional columns can
be omitted by providing `Nothing` instead.  Numeric SQL types have a
Haskell `Num` instance so we can write them using numeric literals.
Values of other types should be created using the functions in the
`Opaleye.PGTypes` module, for example `pgString` to create a `Column
P.PGText` from a `String`.

> insertNothing :: String
> insertNothing = arrangeInsertManySql myTable (return (Nothing, 2, 3, P.pgString "Hello"))

ghci> putStrLn insertNothing
INSERT INTO "tablename" ("id",
                         "x",
                         "y",
                         "s")
VALUES (DEFAULT,
        2.0,
        3.0,
        E'Hello')


If we'd like to pass a value into the insertion function, we can't
rely on the Num instance and must use constant:

> insertNonLiteral :: Double -> String
> insertNonLiteral i =
>   arrangeInsertManySql myTable (return (Nothing, 2, C.constant i, P.pgString "Hello"))

ghci> putStrLn $ insertNonLiteral 12.0
INSERT INTO "tablename" ("id",
                         "x",
                         "y",
                         "s")
VALUES (DEFAULT,
        2.0,
        12.0,
        E'Hello')


If we really want to specify an optional column we can use `Just`.

> insertJust :: String
> insertJust = arrangeInsertManySql myTable (return (Just 1, 2, 3, P.pgString "Hello"))

ghci> putStrLn insertJust
INSERT INTO "tablename" ("id",
                         "x",
                         "y",
                         "s")
VALUES (1,
        2.0,
        3.0,
        E'Hello')


An update takes an update function from the read type to the write
type, and a condition given by a function from the read type to
`Column Bool`.  All rows that satisfy the condition are updated
according to the update function.

> update :: String
> update = arrangeUpdateSql myTable (\(_, x, y, s) -> (Nothing, x + y, x - y, s))
>                                   (\(id_, _, _, _) -> id_ .== 5)

ghci> putStrLn update
SET "id" = DEFAULT,
    "x" = ("x") + ("y"),
    "y" = ("x") - ("y"),
    "s" = "s"
WHERE (("id") = 5)


Sometimes when we insert a row with an automatically generated field
we want the database to return the new field value to us so we can use
it in future queries.  SQL supports that via INSERT RETURNING and
Opaleye supports it also.

> insertReturning :: String
> insertReturning =
>   arrangeInsertManyReturningSql def' myTable (return (Nothing, 4, 5, P.pgString "Bye"))
>                                              (\(id_, _, _, _) -> id_)
>   -- TODO: vv This is too messy
>   where def' :: U.Unpackspec (Column a) (Column a)
>         def' = def

ghci> putStrLn insertReturning
INSERT INTO "tablename" ("id",
                         "x",
                         "y",
                         "s")
VALUES (DEFAULT,
        4.0,
        5.0,
        E'Bye')
RETURNING "id"


Running the queries
===================

This tutorial has only shown you how to generate the SQL string for
manipulation queries.  In practice you actually want to run them!  To
run them you should use `runInsertMany` instead of `arrangeInsertManySql`,
`runDelete` instead of `arrangeDeleteSql`, etc..


Comments
========

Opaleye does not currently support SELECT-valued INSERT or UPDATE.
