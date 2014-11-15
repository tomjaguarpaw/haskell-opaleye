# High priority

* Make a forked HaskellDB package called haskelldb-opaleye that is
  just for the use of Opaleye

# Low priority

* General type families for improving instance resolution (product-profunctors)

## Good starter projects for someone wanting to contribute to Opaleye

### Very easy

* A Prelude module for re-exporting everything
* There may be some missing operators that just need to be written down
* RIGHT JOIN, FULL OUTER JOIN
* Set operations
    * EXCEPT
    * EXCEPT ALL
    * UNION
    * INTERSECT
    * INTERSECT ALL
* INSERT, UPDATE, DELETE RETURNING

### Require a bit of work

* Make VALUES work with more, type checked, value types
* Product-valued case statements
