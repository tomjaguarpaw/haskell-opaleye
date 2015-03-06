# Low priority

* General type families for improving instance resolution
  (product-profunctors).  (Currently there are three places in Opaleye
  where instance resolution needs to happen: `runQuery`,
  `runInsertReturning` and `leftJoin`.  As it happens it seems
  difficult to make any of these benefit from a general type family
  approach.)

## Good starter projects for someone wanting to contribute to Opaleye

### Very easy

* There may be some missing operators that just need to be written down
* RIGHT JOIN, FULL OUTER JOIN
* Set operations
    * EXCEPT
    * EXCEPT ALL
    * UNION
    * INTERSECT
    * INTERSECT ALL
* INSERT, UPDATE, DELETE RETURNING
* Improve the testing "framework" perhaps by upgrading it to Tasty

### Require a bit of work

* Make the code generation neater
* Make VALUES work with more, type checked, value types
* Product-valued case statements and (.==)
* Make the test database parameters more easily configurable
* Randomised testing in a QuickCheck style
* distinct, union and aggregate can be made to work with QueryArr
  rather than just Query if we use LATERAL JOIN
