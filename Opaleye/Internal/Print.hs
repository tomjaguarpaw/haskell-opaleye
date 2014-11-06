module Opaleye.Internal.Print where

import           Prelude hiding (product)

import qualified Opaleye.Internal.SqlOpaleye as Sql
import           Opaleye.Internal.SqlOpaleye (Select(SelectFrom, Table,
                                                     SelectLeftJoin),
                                              From,
                                              LeftJoin)

import qualified Database.HaskellDB.Sql as S
import qualified Database.HaskellDB.Sql.Print as PP

import qualified Text.PrettyPrint.HughesPJ as HPJ
import           Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, empty)

import qualified Data.Maybe as M

ppSql :: Select -> Doc
ppSql (SelectFrom s) = ppSelectFrom s
ppSql (Table name) = text name
ppSql (SelectLeftJoin j) = ppSelectLeftJoin j

ppSelectFrom :: From -> Doc
ppSelectFrom s = text "SELECT"
                 <+> ppAttrs (Sql.attrs s)
                 $$  ppTables (Sql.tables s)
                 $$  PP.ppWhere (Sql.criteria s)
                 $$  ppGroupBy (Sql.groupBy s)
                 $$  PP.ppOrderBy (Sql.orderBy s)
                 $$  ppLimit (Sql.limit s)
                 $$  ppOffset (Sql.offset s)


ppSelectLeftJoin :: LeftJoin -> Doc
ppSelectLeftJoin j = text "SELECT"
                     <+> ppAttrs (Sql.jAttrs j)
                     $$  text "FROM"
                     $$  ppTable (tableAlias 1 s1)
                     $$  text "LEFT OUTER JOIN"
                     $$  ppTable (tableAlias 2 s2)
                     $$  text "ON"
                     $$  PP.ppSqlExpr (Sql.jCond j)
  where (s1, s2) = Sql.jTables j

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

ppTable :: (S.SqlTable, Select) -> Doc
ppTable (alias, select) = case select of
  Table name -> PP.ppAs alias (text name)
  SelectFrom selectFrom -> PP.ppAs alias (HPJ.parens (ppSelectFrom selectFrom))
  SelectLeftJoin slj -> PP.ppAs alias (HPJ.parens (ppSelectLeftJoin slj))

ppGroupBy :: [S.SqlExpr] -> Doc
ppGroupBy [] = empty
ppGroupBy xs = (PP.ppGroupBy . S.Columns . map (\expr -> ("", expr))) xs

ppLimit :: Maybe Int -> Doc
ppLimit Nothing = empty
ppLimit (Just n) = text ("LIMIT " ++ show n)

ppOffset :: Maybe Int -> Doc
ppOffset Nothing = empty
ppOffset (Just n) = text ("OFFSET " ++ show n)
