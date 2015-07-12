* Added `runUpdateReturning`.
* Added string and array aggregations.
* Added `PGOrd` typeclass for typesafe ordering operations.
* Support sorting NULLs first or last
* Added JSON types.
* Added `runInsertMany`

Thanks to Travis Staton, Jakub Ryška and Christopher Lewis for helping
with these changes.

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
