# The design of Opaleye

*DRAFT!*

## Problems with SQL

It's very heavyweight to abstract over anything in SQL.  You can
perhaps use temporary tables and views and you can perhaps use named
columns as "let bindings" but it's all very clumsy.  This means it's
very hard to reuse code.

It's awkward to generate composable SQL strings from another language
because you end up needing things like unique names.

Although you can generate SQL strings at runtime you can't know at
compile time that your SQL is syntactically correct.

Every subselect has to be given a name.  Typically this is redundant.

### SQL language inconsistencies

This orders by the second column

    SELECT * from table ORDER BY 2;

whereas this orders by the value of 1 + 1, i.e. 2.

    SELECT * from table ORDER BY 1 + 1;

## `Query` and `Column`

The most important types in Opaleye are `Query` and `Column`.  A
`Query` represents the result of running a database query, i.e. a
collection of rows with particular column types.  The column types are
specified in the type parameter to `Query` as a collection of
`Column`s.  Each `Column` also has a type parameter reflecting its SQL
type.  For example a `Query (Column PGInt4, Column PGText, Column
PGBool)` is the type of a database query which has three columns, of
types `int4`, `text` and `bool`.

A `Query` is a collection of rows and therefore if we have two of them
we can form their Cartesian product.  This corresponds exactly to
Haskells `Applicative` product on lists.
