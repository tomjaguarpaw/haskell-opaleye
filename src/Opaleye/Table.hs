{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


{- |

 Columns can be required or optional and, independently, nullable or
 non-nullable.

 A required non-nullable @SqlInt4@ (for example) is created with
 'required' and gives rise to a

 @
 TableColumns (Column SqlInt4) (Column SqlInt4)
 @

 The leftmost argument is the type of writes. When you insert or
 update into this column you must give it a @Column SqlInt4@ (which you
 can create with @sqlInt4 :: Int -> Column SqlInt4@).

 A required nullable @SqlInt4@ is created with 'required' and gives rise
 to a

 @
 TableColumns (Column (Nullable SqlInt4)) (Column (Nullable SqlInt4))
 @

 When you insert or update into this column you must give it a @Column
 (Nullable SqlInt4)@, which you can create either with @sqlInt4@ and
 @toNullable :: Column a -> Column (Nullable a)@, or with @null ::
 Column (Nullable a)@.

 An optional non-nullable @SqlInt4@ is created with 'optional' and gives
 rise to a

 @
 TableColumns (Maybe (Column SqlInt4)) (Column SqlInt4)
 @

 Optional columns are those that can be omitted on writes, such as
 those that have @DEFAULT@s or those that are @SERIAL@.
 When you insert or update into this column you must give it a @Maybe
 (Column SqlInt4)@. If you provide @Nothing@ then the column will be
 omitted from the query and the default value will be used. Otherwise
 you have to provide a @Just@ containing a @Column SqlInt4@.

 An optional nullable @SqlInt4@ is created with 'optional' and gives
 rise to a

 @
 TableColumns (Maybe (Column (Nullable SqlInt4))) (Column (Nullable SqlInt4))
 @

 Optional columns are those that can be omitted on writes, such as
 those that have @DEFAULT@s or those that are @SERIAL@.
 When you insert or update into this column you must give it a @Maybe
 (Column (Nullable SqlInt4))@. If you provide @Nothing@ then the default
 value will be used. Otherwise you have to provide a @Just@ containing
 a @Column (Nullable SqlInt4)@ (which can be null).

-}

module Opaleye.Table (-- * Creating tables
                      table,
                      tableWithSchema,
                      T.Table,
                      T.tableColumn,
                      T.optional,
                      T.required,
                      -- * Querying tables
                      selectTable,
                      -- * Other
                      TableColumns,
                      -- * Deprecated
                      View,
                      Writer,
                      T.Table(T.Table, T.TableWithSchema),
                      -- * Module reexport
                      module Opaleye.Table) where

import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Table as T
import           Opaleye.Internal.Table (View, Table, Writer,
                                         TableColumns)

import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Unpackspec as U

import qualified Opaleye.Select                  as S

import qualified Data.Profunctor.Product.Default as D

-- | Example type specialization:
--
-- @
-- selectTable :: Table w (Column a, Column b)
--             -> Select (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- selectTable :: Table w (Foo (Column a) (Column b) (Column c))
--             -> Select (Foo (Column a) (Column b) (Column c))
-- @
selectTable :: D.Default U.Unpackspec columns columns
            => Table a columns
            -- ^
            -> S.Select columns
selectTable = selectTableExplicit D.def

-- | Create a table with unqualified names.
table :: String
      -- ^ Table name
      -> TableColumns writeColumns viewColumns
      -> Table writeColumns viewColumns
table = T.Table

-- | Create a table.
tableWithSchema :: String
                -- ^ Schema name
                -> String
                -- ^ Table name
                -> TableColumns writeColumns viewColumns
                -> Table writeColumns viewColumns
tableWithSchema = T.TableWithSchema

-- * Explicit versions

selectTableExplicit :: U.Unpackspec tablecolumns columns
                    -- ^
                    -> Table a tablecolumns
                    -- ^
                    -> S.Select columns
selectTableExplicit cm table' = Q.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = T.queryTable cm table' t0

-- * Deprecated versions

-- | Use 'selectTable' instead.  Will be deprecated in version 0.7.
queryTable :: D.Default U.Unpackspec columns columns =>
              Table a columns -> S.Select columns
queryTable = selectTable

-- | Use 'selectTableExplicit' instead.  Will be deprecated in version
-- 0.7.
queryTableExplicit :: U.Unpackspec tablecolumns columns ->
                     Table a tablecolumns -> S.Select columns
queryTableExplicit = selectTableExplicit
