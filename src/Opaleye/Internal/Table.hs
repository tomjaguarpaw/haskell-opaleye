{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Opaleye.Internal.Table where

import           Opaleye.Internal.Column (Column, unColumn)
import qualified Opaleye.Internal.TableMaker as TM
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Functor.Identity as I
import           Data.Profunctor (Profunctor, dimap, lmap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import qualified Data.List.NonEmpty as NEL
import           Data.Monoid (Monoid, mempty, mappend)
import           Control.Applicative (Applicative, pure, (<*>), liftA2)
import qualified Control.Arrow as Arr

-- | Define a table as follows, where \"id\", \"color\", \"location\",
-- \"quantity\" and \"radius\" are the tables columns in Postgres and
-- the types are given in the type signature.  The @id@ field is an
-- autoincrementing field (i.e. optional for writes).
--
-- @
-- data Widget a b c d e = Widget { wid      :: a
--                                , color    :: b
--                                , location :: c
--                                , quantity :: d
--                                , radius   :: e }
--
-- $('Data.Profunctor.Product.TH.makeAdaptorAndInstance' \"pWidget\" ''Widget)
--
-- widgetTable :: Table (Widget (Maybe (Column NonNullable PGInt4)) (Column NonNullable PGText)
--                              (Column NonNullable PGText) (Column NonNullable PGInt4)
--                              (Column NonNullable PGFloat8))
--                      (Widget (Column NonNullable PGText) (Column NonNullable PGText)
--                              (Column NonNullable PGText) (Column NonNullable PGInt4)
--                              (Column NonNullable PGFloat8))
-- widgetTable = Table \"widgetTable\"
--                      (pWidget Widget { wid      = optional \"id\"
--                                      , color    = required \"color\"
--                                      , location = required \"location\"
--                                      , quantity = required \"quantity\"
--                                      , radius   = required \"radius\" })
-- @
data Table writerColumns viewColumns
  = Table String (TableProperties writerColumns viewColumns)
    -- ^ Uses the default schema name (@\"public\"@).
  | TableWithSchema String String (TableProperties writerColumns viewColumns)
    -- ^ Schema name (@\"public\"@ by default in PostgreSQL), table name,
    --   table properties.

tableIdentifier :: Table writerColumns viewColumns -> PQ.TableIdentifier
tableIdentifier (Table t _) = PQ.TableIdentifier Nothing t
tableIdentifier (TableWithSchema s t _) = PQ.TableIdentifier (Just s) t

tableProperties :: Table writerColumns viewColumns -> TableProperties writerColumns viewColumns
tableProperties (Table _ p) = p
tableProperties (TableWithSchema _ _ p) = p

data TableProperties writerColumns viewColumns = TableProperties
   { tablePropertiesWriter :: Writer writerColumns viewColumns
   , tablePropertiesView   :: View viewColumns }

data View columns = View columns

-- There's no reason the second parameter should exist except that we
-- use ProductProfunctors more than ProductContravariants so it makes
-- things easier if we make it one of the former.
--
-- Writer has become very mysterious.  I really couldn't tell you what
-- it means.  It seems to be saying that a `Writer` tells you how an
-- `f columns` contains a list of `(f HPQ.PrimExpr, String)`, i.e. how
-- it contains each column: a column header and the entries in this
-- column for all the rows.
newtype Writer columns dummy =
  Writer (forall f. Functor f =>
          PM.PackMap (f HPQ.PrimExpr, String) () (f columns) ())

queryTable :: TM.ColumnMaker viewColumns columns
            -> Table writerColumns viewColumns
            -> Tag.Tag
            -> (columns, PQ.PrimQuery)
queryTable cm table tag = (primExprs, primQ) where
  View tableCols = tablePropertiesView (tableProperties table)
  (primExprs, projcols) = runColumnMaker cm tag tableCols
  primQ :: PQ.PrimQuery
  primQ = PQ.BaseTable (tableIdentifier table) projcols

runColumnMaker :: TM.ColumnMaker tablecolumns columns
                  -> Tag.Tag
                  -> tablecolumns
                  -> (columns, [(HPQ.Symbol, HPQ.PrimExpr)])
runColumnMaker cm tag tableCols = PM.run (TM.runColumnMaker cm f tableCols) where
  f = PM.extractAttrPE mkName tag
  -- The non-AttrExpr PrimExprs are not created by 'makeView' or a
  -- 'ViewColumnMaker' so could only arise from an fmap (if we
  -- implemented a Functor instance) or a direct manipulation of the
  -- tablecols contained in the View (which would be naughty)
  mkName pe i = (++ i) $ case pe of
    HPQ.BaseTableAttrExpr columnName -> columnName
    HPQ.CompositeExpr columnExpr fieldName -> mkName columnExpr i ++ fieldName
    _ -> "tablecolumn"

runWriter :: Writer columns columns' -> columns -> [(HPQ.PrimExpr, String)]
runWriter (Writer (PM.PackMap f)) columns = outColumns
  where (outColumns, ()) = f extract (I.Identity columns)
        extract (pes, s) = ([(I.runIdentity pes, s)], ())

-- This works more generally for any "zippable", that is an
-- Applicative that satisfies
--
--    x == (,) <$> fmap fst x <*> fmap snd x
--
-- However, I'm unaware of a typeclass for this.
runWriter' :: Writer columns columns' -> NEL.NonEmpty columns -> (NEL.NonEmpty [HPQ.PrimExpr], [String])
runWriter' (Writer (PM.PackMap f)) columns = Arr.first unZip outColumns
  where (outColumns, ()) = f extract columns
        extract (pes, s) = ((Zip (fmap return pes), [s]), ())

data Zip a = Zip { unZip :: NEL.NonEmpty [a] }

instance Monoid (Zip a) where
  mempty = Zip mempty'
    where mempty' = [] `NEL.cons` mempty'
  Zip xs `mappend` Zip ys = Zip (NEL.zipWith (++) xs ys)

required :: String -> Writer (Column n a) (Column n a)
required columnName =
  Writer (PM.PackMap (\f columns -> f (fmap unColumn columns, columnName)))

optional :: String -> Writer (Maybe (Column n a)) (Column n a)
optional columnName =
  Writer (PM.PackMap (\f columns -> f (fmap maybeUnColumn columns, columnName)))
  where maybeUnColumn Nothing = HPQ.DefaultInsertExpr
        maybeUnColumn (Just column) = unColumn column

-- {

-- Boilerplate instance definitions

instance Functor (Writer a) where
  fmap _ (Writer g) = Writer g

instance Applicative (Writer a) where
  pure x = Writer (fmap (const ()) (pure x))
  Writer f <*> Writer x = Writer (liftA2 (\_ _ -> ()) f x)

instance Profunctor Writer where
  dimap f _ (Writer h) = Writer (lmap (fmap f) h)

instance ProductProfunctor Writer where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance Functor (TableProperties a) where
  fmap f (TableProperties w (View v)) = TableProperties (fmap f w) (View (f v))

instance Applicative (TableProperties a) where
  pure x = TableProperties (pure x) (View x)
  TableProperties fw (View fv) <*> TableProperties xw (View xv) =
    TableProperties (fw <*> xw) (View (fv xv))

instance Profunctor TableProperties where
  dimap f g (TableProperties w (View v)) = TableProperties (dimap f g w)
                                                            (View (g v))
instance ProductProfunctor TableProperties where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance Functor (Table a) where
  fmap f (Table t tp) = Table t (fmap f tp)
  fmap f (TableWithSchema s t tp) = TableWithSchema s t (fmap f tp)

-- }
