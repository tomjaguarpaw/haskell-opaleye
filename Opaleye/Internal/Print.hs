module Opaleye.Internal.Print where

import           Prelude hiding (product)

import qualified Opaleye.Internal.Sql as Sql
import           Opaleye.Internal.Sql (Select(SelectFrom, Table,
                                              SelectJoin,
                                              SelectValues),
                                       From, Join, Values)

import qualified Database.HaskellDB.Sql as S
import qualified Database.HaskellDB.Sql.Print as PP

import           Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty,
                                            parens)

import qualified Data.Maybe as M

ppSql :: Select -> Doc
ppSql (SelectFrom s) = ppSelectFrom s
ppSql (Table name) = text name
ppSql (SelectJoin j) = ppSelectJoin j
ppSql (SelectValues v) = ppSelectValues v

ppSelectFrom :: From -> Doc
ppSelectFrom s = text "SELECT"
                 <+> ppAttrs (Sql.attrs s)
                 $$  ppTables (Sql.tables s)
                 $$  PP.ppWhere (Sql.criteria s)
                 $$  ppGroupBy (Sql.groupBy s)
                 $$  PP.ppOrderBy (Sql.orderBy s)
                 $$  ppLimit (Sql.limit s)
                 $$  ppOffset (Sql.offset s)


ppSelectJoin :: Join -> Doc
ppSelectJoin j = text "SELECT"
                 <+> ppAttrs (Sql.jAttrs j)
                 $$  text "FROM"
                 $$  ppTable (tableAlias 1 s1)
                 $$  ppJoinType (Sql.jJoinType j)
                 $$  ppTable (tableAlias 2 s2)
                 $$  text "ON"
                 $$  PP.ppSqlExpr (Sql.jCond j)
  where (s1, s2) = Sql.jTables j

ppSelectValues :: Values -> Doc
ppSelectValues v = text "SELECT"
                   <+> ppAttrs (Sql.vAttrs v)
                   $$  text "FROM"
                   $$  ppValues (Sql.vValues v)

ppJoinType :: Sql.JoinType -> Doc
ppJoinType Sql.LeftJoin = text "LEFT OUTER JOIN"

ppAttrs :: [(S.SqlExpr, Maybe S.SqlColumn)] -> Doc
ppAttrs [] = text "*"
ppAttrs xs = PP.commaV nameAs xs

-- This is pretty much just nameAs from HaskellDB
nameAs :: (S.SqlExpr, Maybe S.SqlColumn) -> Doc
nameAs (expr, name) = PP.ppAs (M.fromMaybe "" name) (PP.ppSqlExpr expr)

ppTables :: [Select] -> Doc
ppTables [] = empty
ppTables ts = text "FROM" <+> PP.commaV ppTable (zipWith tableAlias [1..] ts)

tableAlias :: Int -> Select -> (S.SqlTable, Select)
tableAlias i select = ("T" ++ show i, select)

-- TODO: duplication with ppSql
ppTable :: (S.SqlTable, Select) -> Doc
ppTable (alias, select) = case select of
  Table name -> PP.ppAs alias (text name)
  SelectFrom selectFrom -> PP.ppAs alias (parens (ppSelectFrom selectFrom))
  SelectJoin slj -> PP.ppAs alias (parens (ppSelectJoin slj))
  SelectValues slv -> PP.ppAs alias (parens (ppSelectValues slv))

ppGroupBy :: [S.SqlExpr] -> Doc
ppGroupBy [] = empty
ppGroupBy xs = (PP.ppGroupBy . S.Columns . map (\expr -> ("", expr))) xs

ppLimit :: Maybe Int -> Doc
ppLimit Nothing = empty
ppLimit (Just n) = text ("LIMIT " ++ show n)

ppOffset :: Maybe Int -> Doc
ppOffset Nothing = empty
ppOffset (Just n) = text ("OFFSET " ++ show n)

ppValues :: [[S.SqlExpr]] -> Doc
ppValues v = PP.ppAs "V" (parens (text "VALUES" $$ PP.commaV ppValuesRow v))

ppValuesRow :: [S.SqlExpr] -> Doc
ppValuesRow = parens . PP.commaH PP.ppSqlExpr
