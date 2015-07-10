module Opaleye.Internal.Print where

import           Prelude hiding (product)

import qualified Opaleye.Internal.Sql as Sql
import           Opaleye.Internal.Sql (Select(SelectFrom, Table,
                                              SelectJoin,
                                              SelectValues,
                                              SelectBinary),
                                       From, Join, Values, Binary)

import qualified Opaleye.Internal.HaskellDB.Sql as HSql
import qualified Opaleye.Internal.HaskellDB.Sql.Print as HPrint

import           Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty,
                                            parens)
import qualified Data.List.NonEmpty as NEL

type TableAlias = String

ppSql :: Select -> Doc
ppSql (SelectFrom s) = ppSelectFrom s
ppSql (Table table) = HPrint.ppTable table
ppSql (SelectJoin j) = ppSelectJoin j
ppSql (SelectValues v) = ppSelectValues v
ppSql (SelectBinary v) = ppSelectBinary v

ppSelectFrom :: From -> Doc
ppSelectFrom s = text "SELECT"
                 <+> ppAttrs (Sql.attrs s)
                 $$  ppTables (Sql.tables s)
                 $$  HPrint.ppWhere (Sql.criteria s)
                 $$  ppGroupBy (Sql.groupBy s)
                 $$  HPrint.ppOrderBy (Sql.orderBy s)
                 $$  ppLimit (Sql.limit s)
                 $$  ppOffset (Sql.offset s)


ppSelectJoin :: Join -> Doc
ppSelectJoin j = text "SELECT *"
                 $$  text "FROM"
                 $$  ppTable (tableAlias 1 s1)
                 $$  ppJoinType (Sql.jJoinType j)
                 $$  ppTable (tableAlias 2 s2)
                 $$  text "ON"
                 $$  HPrint.ppSqlExpr (Sql.jCond j)
  where (s1, s2) = Sql.jTables j

ppSelectValues :: Values -> Doc
ppSelectValues v = text "SELECT"
                   <+> ppAttrs (Sql.vAttrs v)
                   $$  text "FROM"
                   $$  ppValues (Sql.vValues v)

ppSelectBinary :: Binary -> Doc
ppSelectBinary b = ppSql (Sql.bSelect1 b)
                   $$ ppBinOp (Sql.bOp b)
                   $$ ppSql (Sql.bSelect2 b)

ppJoinType :: Sql.JoinType -> Doc
ppJoinType Sql.LeftJoin = text "LEFT OUTER JOIN"

ppAttrs :: Sql.SelectAttrs -> Doc
ppAttrs Sql.Star             = text "*"
ppAttrs (Sql.SelectAttrs xs) = (HPrint.commaV nameAs . NEL.toList) xs

-- This is pretty much just nameAs from HaskellDB
nameAs :: (HSql.SqlExpr, Maybe HSql.SqlColumn) -> Doc
nameAs (expr, name) = HPrint.ppAs (maybe "" unColumn name) (HPrint.ppSqlExpr expr)
  where unColumn (HSql.SqlColumn s) = s

ppTables :: [Select] -> Doc
ppTables [] = empty
ppTables ts = text "FROM" <+> HPrint.commaV ppTable (zipWith tableAlias [1..] ts)

tableAlias :: Int -> Select -> (TableAlias, Select)
tableAlias i select = ("T" ++ show i, select)

-- TODO: duplication with ppSql
ppTable :: (TableAlias, Select) -> Doc
ppTable (alias, select) = case select of
  Table table -> HPrint.ppAs alias (HPrint.ppTable table)
  SelectFrom selectFrom -> HPrint.ppAs alias (parens (ppSelectFrom selectFrom))
  SelectJoin slj -> HPrint.ppAs alias (parens (ppSelectJoin slj))
  SelectValues slv -> HPrint.ppAs alias (parens (ppSelectValues slv))
  SelectBinary slb -> HPrint.ppAs alias (parens (ppSelectBinary slb))

ppGroupBy :: Maybe (NEL.NonEmpty HSql.SqlExpr) -> Doc
ppGroupBy Nothing   = empty
ppGroupBy (Just xs) = HPrint.ppGroupBy (NEL.toList xs)

ppLimit :: Maybe Int -> Doc
ppLimit Nothing = empty
ppLimit (Just n) = text ("LIMIT " ++ show n)

ppOffset :: Maybe Int -> Doc
ppOffset Nothing = empty
ppOffset (Just n) = text ("OFFSET " ++ show n)

ppValues :: [[HSql.SqlExpr]] -> Doc
ppValues v = HPrint.ppAs "V" (parens (text "VALUES" $$ HPrint.commaV ppValuesRow v))

ppValuesRow :: [HSql.SqlExpr] -> Doc
ppValuesRow = parens . HPrint.commaH HPrint.ppSqlExpr

ppBinOp :: Sql.BinOp -> Doc
ppBinOp o = text $ case o of
  Sql.Union    -> "UNION"
  Sql.UnionAll -> "UNION ALL"
  Sql.Except   -> "EXCEPT"

ppInsertReturning :: Sql.Returning HSql.SqlInsert -> Doc
ppInsertReturning (Sql.Returning insert returnExprs) =
  HPrint.ppInsert insert
  $$ text "RETURNING"
  <+> HPrint.commaV HPrint.ppSqlExpr returnExprs

ppUpdateReturning :: Sql.Returning HSql.SqlUpdate -> Doc
ppUpdateReturning (Sql.Returning update returnExprs) =
  HPrint.ppUpdate update
  $$ text "RETURNING"
  <+> HPrint.commaV HPrint.ppSqlExpr returnExprs
