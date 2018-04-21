* Added `Opaleye.Field`

* `queryTable` is renamed `selectTable`

* `Query`/`QueryArr` are renamed `Select`/`SelectArr`

* Added `Opaleye.SqlTypes` and `sql`/`Sql...` names instead of
  `pg`/`PG...` names

* Added `runInsert_`, `runUpdate_`, `runDelete_` and supporting
  functionality

* Added `leftJoinA`

## 0.6.1.0

* Added `ZonedTime` to `PGTimestamptz` mappings

* `ArrowChoice` instance for `QueryArr`

## 0.6.0.0

* Added `runUpdateEasy`

* Deprecated

  * `Show` instance of `Column a`
  * `Manipulation.arrange...`
  * `showPGType`
  * `literalColumn`
  * `unsafePgFormatTime`
  * `prepareQuery`
  * `formatAndShowSQL`

* Removed

  * `unsafeCoerce`

* Added `TableColumn` and `tableColumn` which selects `optional` or
  `required` based on write type.

* Added `TableColumns` as synonym for `TableProperties`.
  `TableProperties` will be deprecated in version 0.7.

* Added `table` as synonym for `Table`.  `Table` will be deprecated in
  version 0.7.

* Added `tableWithSchema` as synonym for `TableWithSchema`.  `Table`
  will be deprecated in version 0.7.

* Replaced `ColumnMaker` with `Unpackspec`, which is identical to it.

* Added `Profunctor` instance for `Table`

* Added `restrictExists` and `restrictNotExists` as synonyms for
  `exists` and `notExists`.  The latter will be deprecated in version
  0.7.

## 0.5.4.0

* Added cursor interface (`Cursor` and friends)

## 0.5.3.1

* `distinctAggregator`, `joinNullable`, `exists`, `notExists`,
  `index`, `timestamptzAtTimeZone`

## 0.5.3.0

* Added support for range types

## 0.5.2.2

* Corrected fixity for .&&

## 0.5.2.1

* Improved documentation

## 0.5.2.0

* Added `Opaleye.FunctionalJoin`
* Fixed handling of `BinExpr OpIn _ (ListExpr _)` in `defaultSqlExpr`.
* `in_` now actually uses the SQL `IN` operator.
* Added support for `ILIKE`

## 0.5.1.0

* Added
    * support for JSON operators
    * Many improvements to the Haddocks
    * RIGHT and FULL OUTER joins

## 0.5.0.0

* Added
    * `(.===)`, `aggregateOrdered`, `countStar`, `countRows`,
      `quot_`, `rem_`, 'charLength`
    * intersection and except query binary operators
    * `Constant` instances for `Maybe` and lists
    * `runInsertManyReturning`
    * `runQueryFold`

## 0.4.2.0

* Added `.===` and `./==` for comparison of product types
* Added `keepWhen` as an alternative to `restrict`
* Added `constant` conversion to and from Aeson
* Added `pgValueJSON` and `pgValueJSONB`

## 0.4.1.0

* Added `Opaleye.Constant` for lifting constant values
* Support microseconds in `pgLocalTime`, `pgTimeOfDay` and `pgUTCTime`
* Added `unsafeCompositeField` to help with defining composite types
* `Order` is an instance of `Semigroup`

Thanks to Adam Bergmark and Matt Wraith for helping with these
changes.

## 0.4.0.0

* Added `runUpdateReturning`
* Ordering operators and `max` and `min` aggregators are now restricted to a typeclass
* Added `stringAgg` and `arrayAgg` aggregations.
* Added `PGOrd` typeclass for typesafe ordering operations.
* Support sorting NULLs first or last with `ascNullsFirst` and `descNullsFirst`
* Added JSON types
* Added `runInsertMany`

Thanks to Travis Staton, Jakub Ryška and Christopher Lewis for
helping with these changes.

## 0.3.1.2

* Use time >= 1.4 and time-locale-compat

## 0.3.1.1

* Bump time to >= 1.5

## 0.3.1

* SQL code generator escapes column names, so table column names can
  be the same as SQL keywords.
* Add `like` operator
* Add the types `PGCitext`, `PGArray`, `PGBytea`

## 0.3

* Replace `Default QueryRunner` with a new class
  `DefaultQueryRunnerColumn`, migrate with `s/Default
  QueryRunner/DefaultQueryRunnerColumn` and
  `s/def/queryRunnerColumnDefault/`
* Remove `ShowConstant`, use the monomorphic functions defined in the
  new module `Opaleye.PGTypes` instead. You will need to replace
  `Column Bool` with `Column PGBool` etc. in query signatures
* Re-export more modules from `Opaleye`
* Add `boolAnd`, `boolOr,` `max`, and `min` aggregators
* Add `lower` and `upper`
* Add operator fixities
* Add `maybeToNullable`
* Add column instances for `Bool`, `UUID`, `Text`, and `UTCTime`
* Expose fieldQueryRunnerColumn from Opaleye.RunQuery
* Add `unsafeCast`
* Re-export `Unpackspec` from `Opaleye.Manipulation`
