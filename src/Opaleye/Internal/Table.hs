{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

module Opaleye.Internal.Table where

import           Opaleye.Internal.Column (Field_(Column), unColumn)
import qualified Opaleye.Internal.Tag as Tag
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Functor.Identity as I
import           Data.Profunctor (Profunctor, dimap, lmap)
import           Data.Profunctor.Product (ProductProfunctor)
import qualified Data.Profunctor.Product as PP
import qualified Data.List.NonEmpty as NEL
import           Control.Applicative (liftA2)
import qualified Control.Arrow as Arr

-- | Define a table as follows, where \"id\", \"color\", \"location\",
-- \"quantity\" and \"radius\" are the table's fields in Postgres and
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
-- \$('Data.Profunctor.Product.TH.makeAdaptorAndInstanceInferrable' \"pWidget\" ''Widget)
--
-- widgetTable :: Table (Widget (Maybe (Field SqlInt4)) (Field SqlText) (Field SqlText)
--                              (Field SqlInt4) (Field SqlFloat8))
--                      (Widget (Field SqlInt4) (Field SqlText) (Field SqlText)
--                              (Field SqlInt4) (Field SqlFloat8))
-- widgetTable = table \"widgetTable\"
--                      (pWidget Widget { wid      = tableField \"id\"
--                                      , color    = tableField \"color\"
--                                      , location = tableField \"location\"
--                                      , quantity = tableField \"quantity\"
--                                      , radius   = tableField \"radius\" })
-- @
data Table writeFields viewFields
  = Table String (TableFields writeFields viewFields)
  | TableWithSchema String String (TableFields writeFields viewFields)

tableIdentifier :: Table writeColumns viewColumns -> PQ.TableIdentifier
tableIdentifier (Table t _) = PQ.TableIdentifier Nothing t
tableIdentifier (TableWithSchema s t _) = PQ.TableIdentifier (Just s) t

tableColumns :: Table writeColumns viewColumns -> TableFields writeColumns viewColumns
tableColumns (Table _ p) = p
tableColumns (TableWithSchema _ _ p) = p

-- | Use 'tableColumns' instead.  Will be deprecated soon.
tableProperties :: Table writeColumns viewColumns -> TableFields writeColumns viewColumns
tableProperties = tableColumns

data TableFields writeColumns viewColumns = TableFields
   { tablePropertiesWriter :: Writer writeColumns viewColumns
   , tablePropertiesView   :: View viewColumns }

tableColumnsWriter :: TableFields writeColumns viewColumns
                   -> Writer writeColumns viewColumns
tableColumnsWriter = tablePropertiesWriter

tableColumnsView :: TableFields writeColumns viewColumns
                 -> View viewColumns
tableColumnsView = tablePropertiesView

newtype View columns = View columns

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

coerceWriterOutput :: Writer columns dummy -> Writer columns dummy'
coerceWriterOutput (Writer w) = Writer w

-- | 'requiredTableField' is for fields which are not optional.  You
-- must provide them on writes.
requiredTableField :: String -> TableFields (Field_ n a) (Field_ n a)
requiredTableField = lmap Just . optionalTableField

-- | 'optionalTableField' is for fields that you can omit on writes,
-- such as fields which have defaults or which are SERIAL.  Setting
-- the write value to @Nothing@ uses SQL @DEFAULT@ in the generated
-- update.
optionalTableField :: String -> TableFields (Maybe (Field_ n a)) (Field_ n a)
optionalTableField columnName = TableFields
  (optionalW columnName)
  (View (Column (HPQ.BaseTableAttrExpr columnName)))

-- | Don't use 'readOnlyTableField'.  It will be formally deprecated
-- in a future version.  It is broken for updates because it always
-- updates its field with @DEFAULT@ which is very unlikely to be what
-- you want!  For more details see
-- <https://github.com/tomjaguarpaw/haskell-opaleye/issues/447#issuecomment-685617841>.
readOnlyTableField :: String -> TableFields () (Field_ n a)
readOnlyTableField = lmap (const Nothing) . optionalTableField

omitOnWriteTableField :: String -> TableFields () (Field_ n a)
omitOnWriteTableField columnName = TableFields
  (coerceWriterOutput (pure ()))
  (View (Column (HPQ.BaseTableAttrExpr columnName)))

-- | You should not define your own instances of
-- 'InferrableTableField'.
class InferrableTableField w n r
    | w -> n, w -> r where
    -- | Infer either a required ('requiredTableField') or optional
    -- ('optionalTableField') field depending on
    -- the write type.  It's generally more convenient to use this
    -- than 'required' or 'optional' but you do have to provide a type
    -- signature instead.
    tableField  :: String -> TableFields w (Field_ n r)

-- | Equivalent to defining the column with 'requiredTableField'.  If
-- the write type is @Field_ n r@ then the read type is also @Field_ n
-- r@.
instance InferrableTableField (Field_ n r) n r where
    tableField = requiredTableField

-- | Equivalent to defining the column with 'optionalTableField'. If
-- the write type is @Maybe (Field_ n r)@ (i.e. @DEFAULT@ can be
-- written to it) then the write type is @Field_ n r@.
instance InferrableTableField (Maybe (Field_ n r)) n r where
    tableField = optionalTableField

queryTable :: U.Unpackspec viewColumns columns
            -> Table writeColumns viewColumns
            -> Tag.Tag
            -> (columns, PQ.PrimQuery)
queryTable cm table tag = (primExprs, primQ) where
  View tableCols = tableColumnsView (tableColumns table)
  (primExprs, projcols) = runColumnMaker cm tag tableCols
  primQ :: PQ.PrimQuery
  primQ = PQ.BaseTable (tableIdentifier table) projcols

runColumnMaker :: U.Unpackspec tablecolumns columns
                  -> Tag.Tag
                  -> tablecolumns
                  -> (columns, [(HPQ.Symbol, HPQ.PrimExpr)])
runColumnMaker cm tag tableCols = PM.run (U.runUnpackspec cm f tableCols) where
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

newtype Zip a = Zip { unZip :: NEL.NonEmpty [a] }

instance Semigroup (Zip a) where
  Zip xs <> Zip ys = Zip (NEL.zipWith (++) xs ys)

instance Monoid (Zip a) where
  mempty = Zip mempty'
    where mempty' = [] `NEL.cons` mempty'
  mappend = (<>)

optionalW :: String -> Writer (Maybe (Field_ n a)) (Field_ n a)
optionalW columnName =
  Writer (PM.iso (flip (,) columnName . fmap maybeUnColumn) id)
  where maybeUnColumn = maybe HPQ.DefaultInsertExpr unColumn

-- {

-- Boilerplate instance definitions

instance Functor (Writer a) where
  fmap _ (Writer g) = Writer g

instance Applicative (Writer a) where
  pure _ = Writer (pure ())
  Writer f <*> Writer x = Writer (liftA2 (\_ _ -> ()) f x)

instance Profunctor Writer where
  dimap f _ (Writer h) = Writer (lmap (fmap f) h)

instance ProductProfunctor Writer where
  purePP = pure
  (****) = (<*>)

instance Functor (TableFields a) where
  fmap f (TableFields w (View v)) = TableFields (fmap f w) (View (f v))

instance Applicative (TableFields a) where
  pure x = TableFields (pure x) (View x)
  TableFields fw (View fv) <*> TableFields xw (View xv) =
    TableFields (fw <*> xw) (View (fv xv))

instance Profunctor TableFields where
  dimap f g (TableFields w (View v)) = TableFields (dimap f g w)
                                                            (View (g v))
instance ProductProfunctor TableFields where
  purePP = pure
  (****) = (<*>)

instance Functor (Table a) where
  fmap f (Table t tp) = Table t (fmap f tp)
  fmap f (TableWithSchema s t tp) = TableWithSchema s t (fmap f tp)

instance Profunctor Table where
  dimap f g (Table t tp) = Table t (dimap f g tp)
  dimap f g (TableWithSchema s t tp) = TableWithSchema s t (dimap f g tp)

-- }
