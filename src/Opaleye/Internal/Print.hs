{-# LANGUAGE LambdaCase #-}

module Opaleye.Internal.Print where

import           Prelude hiding (product)

import qualified Opaleye.Internal.Sql as Sql
import           Opaleye.Internal.Sql (Select(SelectFrom,
                                              Table,
                                              RelExpr,
                                              SelectJoin,
                                              SelectSemijoin,
                                              SelectValues,
                                              SelectBinary,
                                              SelectLabel,
                                              SelectExists),
                                       From, Join, Semijoin, Values, Binary, Label, Exists)

import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Print as HPrint
import qualified Opaleye.Internal.Optimize as Op
import qualified Opaleye.Internal.Tag as T

import           Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty,
                                            parens)
import qualified Data.Char
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text          as ST

type TableAlias = String

ppSql :: Select -> Doc
ppSql (SelectFrom s)     = ppSelectFrom s
ppSql (Table table)      = HPrint.ppTable table
ppSql (RelExpr expr)     = HPrint.ppSqlExpr expr
ppSql (SelectJoin j)     = ppSelectJoin j
ppSql (SelectSemijoin j) = ppSelectSemijoin j
ppSql (SelectValues v)   = ppSelectValues v
ppSql (SelectBinary v)   = ppSelectBinary v
ppSql (SelectLabel v)    = ppSelectLabel v
ppSql (SelectExists v)   = ppSelectExists v

ppDistinctOn :: Maybe (NEL.NonEmpty HSql.SqlExpr) -> Doc
ppDistinctOn = maybe mempty $ \nel ->
    text "DISTINCT ON" <+>
        text "(" $$ HPrint.commaV HPrint.ppSqlExpr (NEL.toList nel) $$ text ")"

ppSelectFrom :: From -> Doc
ppSelectFrom s = text "SELECT"
                 <+> ppDistinctOn (Sql.distinctOn s)
                 $$  ppAttrs (Sql.attrs s)
                 $$  ppTables (Sql.tables s)
                 $$  HPrint.ppWhere (Sql.criteria s)
                 $$  ppGroupBy (Sql.groupBy s)
                 $$  HPrint.ppOrderBy (Sql.orderBy s)
                 $$  ppLimit (Sql.limit s)
                 $$  ppOffset (Sql.offset s)
                 $$  ppFor (Sql.for s)


ppSelectJoin :: Join -> Doc
ppSelectJoin j = text "SELECT *"
                 $$  text "FROM"
                 $$  ppTable (tableAlias 1 s1)
                 $$  ppJoinType (Sql.jJoinType j)
                 $$  ppTable (tableAlias 2 s2)
                 $$  text "ON"
                 $$  HPrint.ppSqlExpr (Sql.jCond j)
  where (s1, s2) = Sql.jTables j

ppSelectSemijoin :: Semijoin -> Doc
ppSelectSemijoin v =
  text "SELECT *"
  $$  text "FROM"
  $$  ppTable (tableAlias 1 (Sql.sjTable v))
  $$  case Sql.sjType v of
        Sql.Semi -> text "WHERE EXISTS"
        Sql.Anti -> text "WHERE NOT EXISTS"
  $$ parens (ppSql (Sql.sjCriteria v))

ppSelectValues :: Values -> Doc
ppSelectValues v = text "SELECT"
                   <+> ppAttrs (Sql.vAttrs v)
                   $$  text "FROM"
                   $$  ppValues (Sql.vValues v)

ppSelectBinary :: Binary -> Doc
ppSelectBinary b = ppSql (Sql.bSelect1 b)
                   $$ ppBinOp (Sql.bOp b)
                   $$ ppSql (Sql.bSelect2 b)

ppSelectLabel :: Label -> Doc
ppSelectLabel l = text "/*" <+> text (preprocess (Sql.lLabel l)) <+> text "*/"
                  $$ ppSql (Sql.lSelect l)
  where
    preprocess = defuseComments . filter Data.Char.isPrint
    defuseComments = ST.unpack
                   . ST.replace (ST.pack "--") (ST.pack " - - ")
                   . ST.replace (ST.pack "/*") (ST.pack " / * ")
                   . ST.replace (ST.pack "*/") (ST.pack " * / ")
                   . ST.pack

ppSelectExists :: Exists -> Doc
ppSelectExists e =
  text "SELECT EXISTS"
  <+> ppTable (Sql.sqlSymbol (Sql.existsBinding e), Sql.existsTable e)

ppJoinType :: Sql.JoinType -> Doc
ppJoinType Sql.LeftJoin = text "LEFT OUTER JOIN"
ppJoinType Sql.RightJoin = text "RIGHT OUTER JOIN"
ppJoinType Sql.FullJoin = text "FULL OUTER JOIN"

ppAttrs :: Sql.SelectAttrs -> Doc
ppAttrs Sql.Star                 = text "*"
ppAttrs (Sql.SelectAttrs xs)     = (HPrint.commaV nameAs . NEL.toList) xs
ppAttrs (Sql.SelectAttrsStar xs) =
  HPrint.commaV id ((map nameAs . NEL.toList) xs ++ [text "*"])

-- This is pretty much just nameAs from HaskellDB
nameAs :: (HSql.SqlExpr, Maybe HSql.SqlColumn) -> Doc
nameAs (expr, name) = HPrint.ppAs (fmap unColumn name) (HPrint.ppSqlExpr expr)
  where unColumn (HSql.SqlColumn s) = s

ppTables :: [(Sql.Lateral, Select)] -> Doc
ppTables [] = empty
ppTables ts = text "FROM" <+> HPrint.commaV ppTable_tableAlias (zip [1..] ts)
  where ppTable_tableAlias :: (Int, (Sql.Lateral, Select)) -> Doc
        ppTable_tableAlias (i, (lat, select)) =
          lateral lat $ ppTable (tableAlias i select)
        lateral = \case
            Sql.NonLateral -> id
            Sql.Lateral -> (text "LATERAL" $$)

tableAlias :: Int -> Select -> (TableAlias, Select)
tableAlias i select = ("T" ++ show i, select)

-- TODO: duplication with ppSql
ppTable :: (TableAlias, Select) -> Doc
ppTable (alias, select) = HPrint.ppAs (Just alias) $ case select of
  Table table           -> HPrint.ppTable table
  RelExpr expr          -> HPrint.ppSqlExpr expr
  SelectFrom selectFrom -> parens (ppSelectFrom selectFrom)
  SelectJoin slj        -> parens (ppSelectJoin slj)
  SelectSemijoin slj    -> parens (ppSelectSemijoin slj)
  SelectValues slv      -> parens (ppSelectValues slv)
  SelectBinary slb      -> parens (ppSelectBinary slb)
  SelectLabel sll       -> parens (ppSelectLabel sll)
  SelectExists saj      -> parens (ppSelectExists saj)

ppGroupBy :: Maybe (NEL.NonEmpty HSql.SqlExpr) -> Doc
ppGroupBy Nothing   = empty
ppGroupBy (Just xs) = HPrint.ppGroupBy (NEL.toList xs)

ppLimit :: Maybe Int -> Doc
ppLimit Nothing = empty
ppLimit (Just n) = text ("LIMIT " ++ show n)

ppOffset :: Maybe Int -> Doc
ppOffset Nothing = empty
ppOffset (Just n) = text ("OFFSET " ++ show n)

ppFor :: Maybe Sql.LockStrength -> Doc
ppFor Nothing       = empty
ppFor (Just Sql.Update) = text "FOR UPDATE"

ppValues :: [[HSql.SqlExpr]] -> Doc
ppValues v = HPrint.ppAs (Just "V") (parens (text "VALUES" $$ HPrint.commaV HPrint.ppValuesRow v))

ppBinOp :: Sql.BinOp -> Doc
ppBinOp o = text $ case o of
  Sql.Union        -> "UNION"
  Sql.UnionAll     -> "UNION ALL"
  Sql.Except       -> "EXCEPT"
  Sql.ExceptAll    -> "EXCEPT ALL"
  Sql.Intersect    -> "INTERSECT"
  Sql.IntersectAll -> "INTERSECT ALL"

ppInsertReturning :: Sql.Returning HSql.SqlInsert -> Doc
ppInsertReturning (Sql.Returning insert returnExprs) =
  HPrint.ppInsert insert
  $$ text "RETURNING"
  <+> HPrint.commaV HPrint.ppSqlExpr (NEL.toList returnExprs)

ppUpdateReturning :: Sql.Returning HSql.SqlUpdate -> Doc
ppUpdateReturning (Sql.Returning update returnExprs) =
  HPrint.ppUpdate update
  $$ text "RETURNING"
  <+> HPrint.commaV HPrint.ppSqlExpr (NEL.toList returnExprs)

ppDeleteReturning :: Sql.Returning HSql.SqlDelete -> Doc
ppDeleteReturning (Sql.Returning delete returnExprs) =
  HPrint.ppDelete delete
  $$ text "RETURNING"
  <+> HPrint.commaV HPrint.ppSqlExpr (NEL.toList returnExprs)

-- * Bits from "Opaleye.Sql".  They don't really belong here but I
-- * have to put them somewhere.

formatAndShowSQL :: ([HPQ.PrimExpr], PQ.PrimQuery' a, T.Tag) -> Maybe String
formatAndShowSQL = fmap (show . ppSql . Sql.sql) . traverse2Of3 Op.removeEmpty
  where -- Just a lens
        traverse2Of3 :: Functor f => (a -> f b) -> (x, a, y) -> f (x, b, y)
        traverse2Of3 f (x, y, z) = fmap (\y' -> (x, y', z)) (f y)
