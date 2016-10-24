# The design of Opaleye

## `Query` and `Column`

The most important types in Opaleye are `Query` and `Column`.  A
`Query` represents the result of running a database query, i.e. a
collection of rows with particular column types.  The column types are
specified in the type parameter to `Query` as a collection of
`Column`s.  Each `Column` also has a type parameter reflecting its SQL
type.  For example a `Query (Column PGInt4, Column PGText, Column
PGBool)` is the type of a database query which has three columns, of
types `int4`, `text` and `bool`.
