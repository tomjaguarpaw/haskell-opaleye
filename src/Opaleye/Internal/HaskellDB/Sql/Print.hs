-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.Sql.Print (
                                     deliteral,
                                     ppUpdate,
                                     ppDelete,
                                     ppInsert,
                                     ppSqlExpr,
                                     ppWhere,
                                     ppGroupBy,
                                     ppOrderBy,
                                     ppTable,
                                     ppAs,
                                     commaV,
                                     commaH
                                    ) where

import Opaleye.Internal.HaskellDB.Sql (SqlColumn(..), SqlDelete(..),
                               SqlExpr(..), SqlOrder(..), SqlInsert(..),
                               SqlUpdate(..), SqlTable(..), SqlRangeBound(..))
import qualified Opaleye.Internal.HaskellDB.Sql as Sql

import Data.List (intersperse)
import qualified Data.List.NonEmpty as NEL
import Text.PrettyPrint.HughesPJ (Doc, (<+>), ($$), (<>), comma, doubleQuotes,
                                  empty, equals, hcat, hsep, parens, punctuate,
                                  text, vcat, brackets)
import Data.Foldable (toList)

-- Silliness to avoid "ORDER BY 1" etc. meaning order by the first
-- column.  We need an identity function, but due to
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/100 we need
-- to be careful not to be over enthusiastic.  Just apply COALESCE to
-- literals.
deliteral :: SqlExpr -> SqlExpr
deliteral expr@(ConstSqlExpr _) = FunSqlExpr "COALESCE" [expr]
deliteral expr                  = expr

ppWhere :: [SqlExpr] -> Doc
ppWhere [] = empty
ppWhere es = text "WHERE"
             <+> hsep (intersperse (text "AND")
                       (map (parens . ppSqlExpr) es))

ppGroupBy :: [SqlExpr] -> Doc
ppGroupBy es = text "GROUP BY" <+> ppGroupAttrs es
  where
    ppGroupAttrs :: [SqlExpr] -> Doc
    ppGroupAttrs cs = commaV (ppSqlExpr . deliteral) cs

ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy [] = empty
ppOrderBy ord = text "ORDER BY" <+> commaV ppOrd ord
    where
    -- Silliness to avoid "ORDER BY 1" etc. meaning order by the first column
    -- Any identity function will do
    --   ppOrd (e,o) = ppSqlExpr e <+> ppSqlDirection o <+> ppSqlNulls o
      ppOrd (e,o) = ppSqlExpr (deliteral e)
                      <+> ppSqlDirection o
                      <+> ppSqlNulls o

ppSqlDirection :: Sql.SqlOrder -> Doc
ppSqlDirection x = text $ case Sql.sqlOrderDirection x of
  Sql.SqlAsc  -> "ASC"
  Sql.SqlDesc -> "DESC"

ppSqlNulls :: Sql.SqlOrder -> Doc
ppSqlNulls x = text $ case Sql.sqlOrderNulls x of
        Sql.SqlNullsFirst -> "NULLS FIRST"
        Sql.SqlNullsLast  -> "NULLS LAST"

ppAs :: Maybe String -> Doc -> Doc
ppAs Nothing      expr = expr
ppAs (Just alias) expr = expr <+> hsep [text "as", doubleQuotes (text alias)]


ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate table assigns criteria)
        = text "UPDATE" <+> ppTable table
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppSqlExpr e


ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete table criteria) =
    text "DELETE FROM" <+> ppTable table $$ ppWhere criteria


ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsert table names values)
    = text "INSERT INTO" <+> ppTable table
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> commaV (\v -> parens (commaV ppSqlExpr v))
                                  (NEL.toList values)

-- If we wanted to make the SQL slightly more readable this would be
-- one easy place to do it.  Currently we wrap all column references
-- in double quotes in case they are keywords.  However, we should be
-- sure that any column names we generate ourselves are not keywords,
-- so we only need to double quote base table column names.
ppColumn :: SqlColumn -> Doc
ppColumn (SqlColumn s) = doubleQuotes (text s)

-- Postgres treats schema and table names as lower case unless quoted.
ppTable :: SqlTable -> Doc
ppTable st = case sqlTableSchemaName st of
    Just sn -> doubleQuotes (text sn) <> text "." <> tname
    Nothing -> tname
  where
    tname = doubleQuotes (text (sqlTableName st))

ppRangeBound :: Bool -> SqlRangeBound -> Doc
ppRangeBound False (Inclusive a) = text "'[" <> ppSqlExpr a
ppRangeBound False (Exclusive a) = text "'(" <> ppSqlExpr a
ppRangeBound True  (Inclusive a) = text "']" <> ppSqlExpr a
ppRangeBound True  (Exclusive a) = text "')" <> ppSqlExpr a

ppSqlExpr :: SqlExpr -> Doc
ppSqlExpr expr =
    case expr of
      ColumnSqlExpr c     -> ppColumn c
      CompositeSqlExpr s x -> parens (ppSqlExpr s) <> text "." <> text x
      ParensSqlExpr e -> parens (ppSqlExpr e)
      BinSqlExpr op e1 e2 -> ppSqlExpr e1 <+> text op <+> ppSqlExpr e2
      PrefixSqlExpr op e  -> text op <+> ppSqlExpr e
      PostfixSqlExpr op e -> ppSqlExpr e <+> text op
      FunSqlExpr f es     -> text f <> parens (commaH ppSqlExpr es)
      AggrFunSqlExpr f es ord -> text f <> parens (commaH ppSqlExpr es <+> ppOrderBy ord)
      ConstSqlExpr c      -> text c
      CaseSqlExpr cs el   -> text "CASE" <+> vcat (toList (fmap ppWhen cs))
                             <+> text "ELSE" <+> ppSqlExpr el <+> text "END"
          where ppWhen (w,t) = text "WHEN" <+> ppSqlExpr w
                               <+> text "THEN" <+> ppSqlExpr t
      ListSqlExpr es      -> parens (commaH ppSqlExpr (NEL.toList es))
      ParamSqlExpr _ v -> ppSqlExpr v
      PlaceHolderSqlExpr -> text "?"
      CastSqlExpr typ e -> text "CAST" <> parens (ppSqlExpr e <+> text "AS" <+> text typ)
      DefaultSqlExpr    -> text "DEFAULT"
      ArraySqlExpr es -> text "ARRAY" <> brackets (commaH ppSqlExpr es)
      RangeSqlExpr start end -> commaH (uncurry ppRangeBound) [(False, start), (True, end)]

commaH :: (a -> Doc) -> [a] -> Doc
commaH f = hcat . punctuate comma . map f

commaV :: (a -> Doc) -> [a] -> Doc
commaV f = vcat . punctuate comma . map f
