-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

{-# LANGUAGE LambdaCase #-}

module Opaleye.Internal.HaskellDB.Sql.Default  where

import Control.Applicative ((<$>))

import Opaleye.Internal.HaskellDB.PrimQuery
import qualified Opaleye.Internal.HaskellDB.PrimQuery as PQ
import Opaleye.Internal.HaskellDB.Sql
import Opaleye.Internal.HaskellDB.Sql.Generate
import qualified Opaleye.Internal.HaskellDB.Sql as Sql
import Opaleye.Internal.Tag (tagWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Char
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Lazy.Builder.Scientific as Sci
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Text.Printf


mkSqlGenerator :: SqlGenerator -> SqlGenerator
mkSqlGenerator gen = SqlGenerator
    {
     sqlUpdate      = defaultSqlUpdate      gen,
     sqlDelete      = defaultSqlDelete      gen,
     sqlInsert      = defaultSqlInsert      gen,
     sqlExpr        = defaultSqlExpr        gen,
     sqlLiteral     = defaultSqlLiteral     gen,
     sqlQuote       = defaultSqlQuote       gen
    }

defaultSqlGenerator :: SqlGenerator
defaultSqlGenerator = mkSqlGenerator defaultSqlGenerator


toSqlOrder :: SqlGenerator -> OrderExpr -> (SqlExpr,SqlOrder)
toSqlOrder gen (OrderExpr o e) =
  (sqlExpr gen e, Sql.SqlOrder { sqlOrderDirection = o'
                               , sqlOrderNulls     = orderNulls' })
    where o' = case PQ.orderDirection o of
            PQ.OpAsc  -> Sql.SqlAsc
            PQ.OpDesc -> Sql.SqlDesc
          orderNulls' = case PQ.orderNulls o of
            PQ.NullsFirst -> Sql.SqlNullsFirst
            PQ.NullsLast  -> Sql.SqlNullsLast


toSqlPartition :: SqlGenerator -> Partition -> SqlPartition
toSqlPartition gen (Partition partition order) = SqlPartition
  { sqlPartitionBy = NEL.nonEmpty (map (sqlExpr gen) partition)
  , sqlOrderBy = NEL.nonEmpty (map (toSqlOrder gen) order)
  }


toSqlColumn :: Attribute -> SqlColumn
toSqlColumn = SqlColumn

toSqlAssoc :: SqlGenerator -> Assoc -> [(SqlColumn,SqlExpr)]
toSqlAssoc gen = map (\(attr,expr) -> (toSqlColumn attr, sqlExpr gen expr))


defaultSqlUpdate :: SqlGenerator
                 -> SqlTable   -- ^ Table to update
                 -> [PrimExpr] -- ^ Conditions which must all be true for a row
                               --   to be updated.
                 -> Assoc -- ^ Update the data with this.
                 -> SqlUpdate
defaultSqlUpdate gen tbl criteria assigns
        = SqlUpdate tbl (toSqlAssoc gen assigns) (map (sqlExpr gen) criteria)


defaultSqlInsert :: SqlGenerator
                 -> SqlTable
                 -> [Attribute]
                 -> NEL.NonEmpty [PrimExpr]
                 -> Maybe OnConflict
                 -> SqlInsert
defaultSqlInsert gen tbl attrs exprs =
  SqlInsert tbl (map toSqlColumn attrs) ((fmap . map) (sqlExpr gen) exprs)

defaultSqlDelete :: SqlGenerator
                 -> SqlTable
                 -> [PrimExpr] -- ^ Criteria which must all be true for a row
                               --   to be deleted.
                 -> SqlDelete
defaultSqlDelete gen tbl criteria = SqlDelete tbl (map (sqlExpr gen) criteria)


defaultSqlExpr :: SqlGenerator -> PrimExpr -> SqlExpr
defaultSqlExpr gen expr =
    case expr of
      AttrExpr (Symbol a t) -> ColumnSqlExpr (SqlColumn (tagWith t a))
      BaseTableAttrExpr a -> ColumnSqlExpr (SqlColumn a)
      CompositeExpr e x -> CompositeSqlExpr (defaultSqlExpr gen e) x
      BinExpr op e1 e2 ->
        let leftE = sqlExpr gen e1
            rightE = sqlExpr gen e2
            paren = ParensSqlExpr
            (expL, expR) = case (op, e1, e2) of
              (OpAnd, BinExpr OpOr _ _, BinExpr OpOr _ _) ->
                (paren leftE, paren rightE)
              (OpOr, BinExpr OpAnd _ _, BinExpr OpAnd _ _) ->
                (paren leftE, paren rightE)
              (OpAnd, BinExpr OpOr _ _, _) ->
                (paren leftE, rightE)
              (OpAnd, _, BinExpr OpOr _ _) ->
                (leftE, paren rightE)
              (OpOr, BinExpr OpAnd _ _, _) ->
                (paren leftE, rightE)
              (OpOr, _, BinExpr OpAnd _ _) ->
                (leftE, paren rightE)
              (OpIn, _, ListExpr _) ->
                (leftE, rightE)
              (_, ConstExpr _, ConstExpr _) ->
                (leftE, rightE)
              (_, _, ConstExpr _) ->
                (paren leftE, rightE)
              (_, ConstExpr _, _) ->
                (leftE, paren rightE)
              _ -> (paren leftE, paren rightE)
        in BinSqlExpr (showBinOp op) expL expR
      UnExpr op e      -> let (op',t) = sqlUnOp op
                              e' = sqlExpr gen e
                           in case t of
                                UnOpFun     -> FunSqlExpr op' [e']
                                UnOpPrefix  -> PrefixSqlExpr op' (ParensSqlExpr e')
                                UnOpPostfix -> PostfixSqlExpr op' (ParensSqlExpr e')
      AggrExpr (Aggr op e ord distinct group mfilter) ->
        let
          (op', e') = showAggrOp gen op e
          ord' = toSqlOrder gen <$> ord
          distinct' = case distinct of
            AggrDistinct -> SqlDistinct
            AggrAll      -> SqlNotDistinct
          group' = toSqlOrder gen <$> group
          mfilter' = sqlExpr gen <$> mfilter
         in AggrFunSqlExpr op' e' ord' distinct' group' mfilter'
      WndwExpr op window  -> let (op', e') = showWndwOp gen op
                                 window' = toSqlPartition gen window
                              in WndwFunSqlExpr op' e' window'
      ConstExpr l      -> ConstSqlExpr (sqlLiteral gen l)
      CaseExpr cs e    -> let cs' = [(sqlExpr gen c, sqlExpr gen x)| (c,x) <- cs]
                              e'  = sqlExpr gen e
                          in case NEL.nonEmpty cs' of
                            Just nel -> CaseSqlExpr nel e'
                            Nothing  -> e'
      ListExpr es      -> ListSqlExpr (fmap (sqlExpr gen) es)
      ParamExpr n _    -> ParamSqlExpr n PlaceHolderSqlExpr
      FunExpr n exprs  -> FunSqlExpr n (map (sqlExpr gen) exprs)
      CastExpr typ e1 -> CastSqlExpr typ (sqlExpr gen e1)
      DefaultInsertExpr -> DefaultSqlExpr
      ArrayExpr es -> ArraySqlExpr (map (sqlExpr gen) es)
      RangeExpr t l r -> let bound :: PQ.BoundExpr -> Sql.SqlRangeBound
                             bound (PQ.Inclusive a) = Sql.Inclusive (sqlExpr gen a)
                             bound (PQ.Exclusive a) = Sql.Exclusive (sqlExpr gen a)
                             bound PQ.PosInfinity   = Sql.PosInfinity
                             bound PQ.NegInfinity   = Sql.NegInfinity
                        in RangeSqlExpr t (bound l) (bound r)
      ArrayIndex e1 e2 -> SubscriptSqlExpr (ParensSqlExpr $ sqlExpr gen e1) (ParensSqlExpr $ sqlExpr gen e2)

showBinOp :: BinOp -> String
showBinOp  (:==)        = "="
showBinOp  (:<)         = "<"
showBinOp  (:<=)        = "<="
showBinOp  (:>)         = ">"
showBinOp  (:>=)        = ">="
showBinOp  (:<>)        = "<>"
showBinOp  OpAnd        = "AND"
showBinOp  OpOr         = "OR"
showBinOp  OpLike       = "LIKE"
showBinOp  OpILike      = "ILIKE"
showBinOp  OpIn         = "IN"
showBinOp  (OpOther s)  = s
showBinOp  (:||)        = "||"
showBinOp  (:+)         = "+"
showBinOp  (:-)         = "-"
showBinOp  (:*)         = "*"
showBinOp  (:/)         = "/"
showBinOp  OpMod        = "MOD"
showBinOp  (:~)         = "~"
showBinOp  (:&)         = "&"
showBinOp  (:|)         = "|"
showBinOp  (:^)         = "^"
showBinOp  (:=)         = "="
showBinOp  OpAtTimeZone = "AT TIME ZONE"
showBinOp  (:->)        = "->"
showBinOp  (:->>)       = "->>"
showBinOp  (:#>)        = "#>"
showBinOp  (:#>>)       = "#>>"
showBinOp  (:@>)        = "@>"
showBinOp  (:<@)        = "<@"
showBinOp  (:?)         = "?"
showBinOp  (:?|)        = "?|"
showBinOp  (:?&)        = "?&"
showBinOp  (:&&)        = "&&"
showBinOp  (:<<)        = "<<"
showBinOp  (:>>)        = ">>"
showBinOp  (:&<)        = "&<"
showBinOp  (:&>)        = "&>"
showBinOp  (:-|-)       = "-|-"

data UnOpType = UnOpFun | UnOpPrefix | UnOpPostfix

sqlUnOp :: UnOp -> (String,UnOpType)
sqlUnOp  OpNot         = ("NOT", UnOpPrefix)
sqlUnOp  OpIsNull      = ("IS NULL", UnOpPostfix)
sqlUnOp  OpIsNotNull   = ("IS NOT NULL", UnOpPostfix)
sqlUnOp  OpLength      = ("LENGTH", UnOpFun)
sqlUnOp  OpAbs         = ("@", UnOpFun)
sqlUnOp  OpNegate      = ("-", UnOpFun)
sqlUnOp  OpLower       = ("LOWER", UnOpFun)
sqlUnOp  OpUpper       = ("UPPER", UnOpFun)
sqlUnOp  (UnOpOther s) = (s, UnOpFun)


showAggrOp :: SqlGenerator -> AggrOp -> [PrimExpr] -> (String, [SqlExpr])
showAggrOp gen op args = (showAggrOpFunction op, map (sqlExpr gen) args)


showAggrOpFunction :: AggrOp -> String
showAggrOpFunction = \case
  AggrCount -> "COUNT"
  AggrSum -> "SUM"
  AggrAvg -> "AVG"
  AggrMin -> "MIN"
  AggrMax -> "MAX"
  AggrStdDev -> "StdDev"
  AggrStdDevP -> "StdDevP"
  AggrVar -> "Var"
  AggrVarP -> "VarP"
  AggrBoolAnd -> "BOOL_AND"
  AggrBoolOr -> "BOOL_OR"
  AggrArr -> "ARRAY_AGG"
  JsonArr -> "JSON_AGG"
  AggrStringAggr -> "STRING_AGG"
  AggrOther s -> s


showWndwOp :: SqlGenerator -> WndwOp -> (String, [SqlExpr])
showWndwOp gen op = case op of
  WndwRowNumber -> ("ROW_NUMBER", [])
  WndwRank -> ("RANK", [])
  WndwDenseRank -> ("DENSE_RANK", [])
  WndwPercentRank -> ("PERCENT_RANK", [])
  WndwCumeDist -> ("CUME_DIST", [])
  WndwNtile e -> ("NTILE", [sqlExpr gen e])
  WndwLag e offset def -> ("LAG", map (sqlExpr gen) [e, offset, def])
  WndwLead e offset def -> ("LEAD", map (sqlExpr gen) [e, offset, def])
  WndwFirstValue e -> ("FIRST_VALUE", [sqlExpr gen e])
  WndwLastValue e -> ("LAST_VALUE", [sqlExpr gen e])
  WndwNthValue e n -> ("NTH_VALUE", map (sqlExpr gen) [e, n])
  WndwAggregate op' args -> showAggrOp gen op' args


defaultSqlLiteral :: SqlGenerator -> Literal -> String
defaultSqlLiteral _ l =
    case l of
      NullLit       -> "NULL"
      DefaultLit    -> "DEFAULT"
      BoolLit True  -> "TRUE"
      BoolLit False -> "FALSE"
      ByteStringLit s
                    -> binQuote s
      StringLit s   -> quote s
      IntegerLit i  -> show i
      DoubleLit d   -> if isNaN d then "'NaN'"
                       else if isInfinite d && d < 0 then "'-Infinity'"
                       else if isInfinite d && d > 0 then "'Infinity'"
                       else show d
      NumericLit n  -> LT.unpack . LT.toLazyText . Sci.scientificBuilder $ n
      OtherLit o    -> o


defaultSqlQuote :: SqlGenerator -> String -> String
defaultSqlQuote _ = quote

-- | Quote a string and escape characters that need escaping
--   We use Postgres "escape strings", i.e. strings prefixed
--   with E, to ensure that escaping with backslash is valid.
quote :: String -> String
quote s = "E'" ++ concatMap escape s ++ "'"

-- | Escape characters that need escaping
escape :: Char -> String
escape '\NUL' = "\\0"
escape '\'' = "''"
escape '"' = "\\\""
escape '\b' = "\\b"
escape '\n' = "\\n"
escape '\r' = "\\r"
escape '\t' = "\\t"
escape '\\' = "\\\\"
escape c = if Data.Char.isPrint c
           then [c]
           else Text.Printf.printf "\\U%0.8x" (Data.Char.ord c)


-- | Quote binary literals using Postgresql's hex format.
binQuote :: ByteString -> String
binQuote s = "E'\\\\x" ++ BS8.unpack (Base16.encode s) ++ "'"
