# Table of Contents

* Instant gratification
  * Simplest way to setup your DB <=> Opaleye mapping
  * Inserting a row
  * Updating a single row
  * Updating multiple rows matching a condition
  * Selecting rows matching a condition
  * Deleting multiple rows matching a condition
  * Joining tables
* Opaleye basics (overall stuff that is required to understand why the library is designed the way it is)
  * Why does Opelaye have a PG-read and a PG-write data-type for every table mapping?
  * Why is it recommended to parameterize records over field types?
  * What's with the weird arrow notation? Why not <insert X here>?
* Migrations - or the lack of them
  * Why doesn't Opaleye handle migrations automatically?
  * Recommendations on which library to use for writing migrations
* Creating DB <=> Opaleye mappings
  * Without typeclasses
  * With typeclasses
  * With TemplateHaskell (??)
  * What does makeAdapatorAndInstance do? (show and explain generated code)
  * Defining required, optional, nuallable columns - and all combinations thereof
  * How does Opaleye know how to convert PG-types => Haskell, and vice versa
  * Mapping ENUMs
  * Mapping custom data types
  * Defining columns as readonly
* Inserting rows
  * Inserting required, optional, nullable columns - and all combinations thereof
  * Inserting a row and returning updated row back
  * Inserting multiple rows and return all updated rows back
* Updating rows
  * Updating required, optional, nullable columns - and all combinations thereof
  * Updates where a column is a complicated formula based on several other columns
  * Update with a JOIN
  * Update multiple rows and return all updated rows back
* Selecting rows
  * Select single row
  * Select single row in batches (recommended for large result sets)
  * Conditions
    * Probably link off to the Hackage docs
  * Limits & Offsets
  * Ordering
  * Joins - all supported kinds
  * Grouping and aggregates
  * Joining and returning results as a nested object graph
