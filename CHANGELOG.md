* Add `Opaleye.MaybeFields`

* Add `optionalTableField`, `readOnlyTableField`,
  `requiredTableField`, to replace `optional`, `readOnly` and
  `required` in a later version.

* Add `valuesSafe`, a version of `values`. `values` of an empty list
  generates incorrect queries when mixed with @OUTER@/@LEFT@/@RIGHT
  JOIN@s.  `valuesSafe` will replace it in version 0.7

* Add `Opaleye.Unpackspec` as the forward-compatible place to import
  `Unpackspec` from

* Unicode characters are escaped properly in `sqlString`/`toFields`

* Add `inSelect`, to replace `inQuery` in a future version.

* Add `unsafeCoerceField`, to replace `unsafeCoerceColumn` in a future
  version.

* Generalise label to type `label :: String -> S.SelectArr a b ->
S.SelectArr a b`

* [Fix invalid queries
  bug](https://github.com/tomjaguarpaw/haskell-opaleye/pull/468) in
  `union`, `unionAll`, `except` and `exceptAll` where one side was
  empty.

## 0.6.7004.2

* No user-visible changes

## 0.6.7004.1

* Fixed exponential slowdown in `removeEmpty`.

* Fixed `read` compatibility with time-1.9 in test suite.

## 0.6.7004.0

* Many changes to the documentation that use the new names.  See entry
  for version 0.6.7000.0.

* Added `fromPGSFromField` to replace `fieldQueryRunnerColumn`.

* Added `fromPGSFieldParser` to replace `fieldParserQueryRunnerColumn`.

* Added `defaultFromField` to replace `queryRunnerColumnDefault`.

* Added `tableField` to replace `tableColumn`.

* Added `unsafeFromField` to replace `queryRunnerColumn`.

* Added `toFieldsExplicit` to replace `constantExplicit`.

* Added `TableRecordField` to replace `TableField` in
  `Opaleye.TypeFamilies`.  The latter may be used to replace
  `TableColumn` in the future.

* Added array functions `arrayAppend`, `arrayRemove`, `arrayRemoveNulls`.

## 0.6.7003.1

* Bumped some depedencies so there is an install plan on GHC 8.6

## 0.6.7003.0

* Add `tableField` as a future replacement for `tableColumn`

* Export `Opaleye.Field` and `Opaleye.RunSelect` from `Opaleye`

* Use new nomenclature in tutorials

## 0.6.7002.0

This is a breaking release that doesn't follow the PVP but because
it's essentially a pre-release for version 0.7 I'm just going to
blacklist the broken versions on Hackage and forget about it.

* Swapped `N` and `NN` because they were the wrong way round.

## 0.6.7001.0

* Fix bug with infinity in range bounds

* Fix incompatibility with GHC 8.4

* Add range accessors, `upperBound` and `lowerBound`

* Add `distinctOn` and `distinctOnBy`

## 0.6.7000.0

This is a pre-release of version 0.7.0.0.  GHC >= 8.0 is required.  It
contains the following new important features

* A new API for manipulation, including `ON CONFLICT DO NOTHING`
  support for `UPDATE`

* Initial support for product types written in "Higher kinded data"
  style (but deriving of related functionality using TH or Generics is
  not yet provided).

* Type inference for outer joins

* Many renamings.  In particular, `Column` will become `Field` in
  0.7.0.0.  You should be able to almost completely port your code to
  the 0.7.0.0 names whilst remaining compatible with 0.6.7000.0.

### Details

* Added `Opaleye.RunSelect`

* Added `Opaleye.Field`

* `queryTable` is renamed `selectTable`

* `Query`/`QueryArr` are renamed `Select`/`SelectArr`

* `QueryRunner` is renamed `FromFields`

* `QueryRunnerColumn` is renamed `FromField`

* `Constant` is renamed `ToFields`

* `constant` is renamed `toFields`

* Added `Opaleye.SqlTypes` and `sql`/`Sql...` names instead of
  `pg`/`PG...` names

* Added `runInsert_`, `runUpdate_`, `runDelete_` and supporting
  functionality

* Add `PGNumeric` type

* Added `leftJoinA`

* Added `liesWithin`

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
