{-# LANGUAGE FlexibleContexts #-}

{- |

 Columns can be required or optional and, independently, nullable or
 non-nullable.

 A required non-nullable @PGInt4@ (for example) is created with
 'required' and gives rise to a

 @
 TableColumns (Column PGInt4) (Column PGInt4)
 @

 The leftmost argument is the type of writes. When you insert or
 update into this column you must give it a @Column PGInt4@ (which you
 can create with @pgInt4 :: Int -> Column PGInt4@).

 A required nullable @PGInt4@ is created with 'required' and gives rise
 to a

 @
 TableColumns (Column (Nullable PGInt4)) (Column (Nullable PGInt4))
 @

 When you insert or update into this column you must give it a @Column
 (Nullable PGInt4)@, which you can create either with @pgInt4@ and
 @toNullable :: Column a -> Column (Nullable a)@, or with @null ::
 Column (Nullable a)@.

 An optional non-nullable @PGInt4@ is created with 'optional' and gives
 rise to a

 @
 TableColumns (Maybe (Column PGInt4)) (Column PGInt4)
 @

 When you insert or update into this column you must give it a @Maybe
 (Column PGInt4)@. If you provide @Nothing@ then the column will be
 omitted from the query and the default value will be used. Otherwise
 you have to provide a @Just@ containing a @Column PGInt4@.

 An optional nullable @PGInt4@ is created with 'optional' and gives
 rise to a

 @
 TableColumns (Maybe (Column (Nullable PGInt4))) (Column (Nullable PGInt4))
 @

 When you insert or update into this column you must give it a @Maybe
 (Column (Nullable PGInt4))@. If you provide @Nothing@ then the default
 value will be used. Otherwise you have to provide a @Just@ containing
 a @Column (Nullable PGInt4)@ (which can be null).

-}

module Opaleye.Table (module Opaleye.Table,
                      -- * Other
                      View,
                      Writer,
                      T.Table(T.Table, T.TableWithSchema),
                      TableColumns) where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.Table as T
import           Opaleye.Internal.Table (View(View), Table, Writer,
                                         TableColumns)
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Unpackspec as U

import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

-- | Example type specialization:
--
-- @
-- queryTable :: Table w (Column a, Column b) -> Query (Column a, Column b)
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the
-- product type @Foo@:
--
-- @
-- queryTable :: Table w (Foo (Column a) (Column b) (Column c)) -> Query (Foo (Column a) (Column b) (Column c))
-- @
queryTable :: D.Default U.Unpackspec columns columns =>
              Table a columns -> Q.Query columns
queryTable = queryTableExplicit D.def

-- | For tables with unqualified names
table :: String
      -- ^^ Table name
      -> TableColumns writerColumns viewColumns
      -> Table writerColumns viewColumns
table = T.Table

tableWithSchema :: String
                -- ^^ Schema name
                -> String
                -- ^^ Table name
                -> TableColumns writerColumns viewColumns
                -> Table writerColumns viewColumns
tableWithSchema = T.TableWithSchema

-- | 'required' is for columns which are not 'optional'.  You must
-- provide them on writes.
required :: String -> TableColumns (Column a) (Column a)
required columnName = T.TableProperties
  (T.required columnName)
  (View (Column (HPQ.BaseTableAttrExpr columnName)))

-- | 'optional' is for columns that you can omit on writes, such as
--  columns which have defaults or which are SERIAL.
optional :: String -> TableColumns (Maybe (Column a)) (Column a)
optional columnName = T.TableProperties
  (T.optional columnName)
  (View (Column (HPQ.BaseTableAttrExpr columnName)))

-- * Explicit versions

queryTableExplicit :: U.Unpackspec tablecolumns columns ->
                     Table a tablecolumns -> Q.Query columns
queryTableExplicit cm table' = Q.simpleQueryArr f where
  f ((), t0) = (retwires, primQ, Tag.next t0) where
    (retwires, primQ) = T.queryTable cm table' t0
