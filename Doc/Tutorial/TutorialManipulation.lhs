> module TutorialManipulation where
>
> import           Prelude hiding (sum)
>
> import           Opaleye (Field, Table, table,
>                           tableField, (.==), (.<),
>                           Insert(..),
>                           Update(..),
>                           Delete(..),
>                           rCount,
>                           rReturning,
>                           updateEasy,
>                           SqlInt4, SqlFloat8, SqlText,
>                           sqlString
>                          )
>
> import           Data.Profunctor.Product (p4)
> import           Opaleye.ToFields (toFields)
>
> import           GHC.Int (Int64)

Manipulation
============

Manipulation means changing the data in the database.  This means SQL
DELETE, INSERT and UPDATE.

To demonstrate manipulation in Opaleye we will need a table to perform
our manipulation on.  It will have four fields: an int4-valued "id"
field (assumed to be an auto-incrementing field) and three
float8-valued required fields.  The `Table` type constructor has two
type arguments.  The first one is the type of writes to the table, and
the second is the type of reads from the table.  The "id" field is
defined as optional (for writes) because its write type is `Maybe
(Field SqlInt4)`.  That means we don't necessarily need to specify it
when writing to the table.  The database will automatically fill in a
value for us.

> myTable :: Table
>     (Maybe (Field SqlInt4), Field SqlFloat8, Field SqlFloat8, Field SqlText)
>     (Field SqlInt4, Field SqlFloat8, Field SqlFloat8, Field SqlText)
> myTable = table "tablename" (p4 ( tableField "id"
>                                 , tableField "x"
>                                 , tableField "y"
>                                 , tableField "s" ))

To perform a delete we provide an expression from our read type to
`Field SqlBool`.  All rows for which the expression is true are deleted.

> delete :: Delete Int64
> delete = Delete
>   { dTable     = myTable
>   , dWhere     = \(_, x, y, _) -> x .< y
>   , dReturning = rCount
>   }

DELETE FROM tablename
WHERE ((x) < (y))


To insert we provide rows with the write type.  Optional fields can
be omitted by providing `Nothing` instead.  Numeric SQL types have a
Haskell `Num` instance so we can write them using numeric literals.
Values of other types should be created using the functions in the
`Opaleye.SqlTypes` module, for example `sqlString` to create a `SqlText`
from a `String`.

> insertNothing :: Insert Int64
> insertNothing = Insert
>   { iTable      = myTable
>   , iRows       = [(Nothing, 2, 3, sqlString "Hello")]
>   , iReturning  = rCount
>   , iOnConflict = Nothing
>   }

INSERT INTO "tablename" ("id",
                         "x",
                         "y",
                         "s")
VALUES (DEFAULT,
        2.0,
        3.0,
        E'Hello')


If we'd like to pass a variable into the insertion function, we can't
rely on the `Num` instance and must use `toFields`:

> insertNonLiteral :: Double -> Insert Int64
> insertNonLiteral i = Insert
>   { iTable      = myTable
>   , iRows       = [(Nothing, 2, toFields i, sqlString "Hello")]
>   , iReturning  = rCount
>   , iOnConflict = Nothing
>   }

INSERT INTO "tablename" ("id",
                         "x",
                         "y",
                         "s")
VALUES (DEFAULT,
        2.0,
        12.0,
        E'Hello')


If we really want to specify an optional field we can use `Just`.

> insertJust :: Insert Int64
> insertJust = Insert
>   { iTable      = myTable
>   , iRows       = [(Just 1, 2, 3, sqlString "Hello")]
>   , iReturning  = rCount
>   , iOnConflict = Nothing
>   }

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
`Field SqlBool`.  All rows that satisfy the condition are updated
according to the update function.

> update :: Update Int64
> update = Update
>   { uTable      = myTable
>   , uUpdateWith = updateEasy (\(id_, x, y, s) -> (id_, x + y, x - y, s))
>   , uWhere      = \(id_, _, _, _) -> id_ .== 5
>   , uReturning  = rCount
>   }

SET "id" = DEFAULT,
    "x" = ("x") + ("y"),
    "y" = ("x") - ("y"),
    "s" = "s"
WHERE (("id") = 5)


Sometimes when we insert a row with an automatically generated field
we want the database to return the new field value to us so we can use
it in future queries.  SQL supports that via `INSERT RETURNING` and
Opaleye supports it also.

> insertReturning :: Insert [Int]
> insertReturning = Insert
>   { iTable      = myTable
>   , iRows       = [(Nothing, 4, 5, sqlString "Bye")]
>   , iReturning  = rReturning (\(id_, _, _, _) -> id_)
>   , iOnConflict = Nothing
>   }

INSERT INTO "tablename" ("id",
                         "x",
                         "y",
                         "s")
VALUES (DEFAULT,
        4.0,
        5.0,
        E'Bye')
RETURNING "id"


Comments
========

Opaleye does not currently support SELECT-valued INSERT or UPDATE.
