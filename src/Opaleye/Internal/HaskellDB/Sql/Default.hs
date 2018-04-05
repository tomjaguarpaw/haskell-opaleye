-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

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
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Lazy.Builder.Scientific as Sci
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT


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
      -- TODO: The current arrangement whereby the delimeter parameter
      -- of string_agg is in the AggrStringAggr constructor, but the
      -- parameter being aggregated is not, seems unsatisfactory
      -- because it leads to a non-uniformity of treatment, as seen
      -- below.  Perhaps we should have just `AggrExpr AggrOp` and
      -- always put the `PrimExpr` in the `AggrOp`.
      AggrExpr distinct op e ord -> let op' = showAggrOp op
                                        e' = sqlExpr gen e
                                        ord' = toSqlOrder gen <$> ord
                                        distinct' = case distinct of
                                                      AggrDistinct -> SqlDistinct
                                                      AggrAll      -> SqlNotDistinct
                                        moreAggrFunParams = case op of
                                          AggrStringAggr primE -> [sqlExpr gen primE]
                                          _ -> []
                                     in AggrFunSqlExpr op' (e' : moreAggrFunParams) ord' distinct'
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


showAggrOp :: AggrOp -> String
showAggrOp AggrCount          = "COUNT"
showAggrOp AggrSum            = "SUM"
showAggrOp AggrAvg            = "AVG"
showAggrOp AggrMin            = "MIN"
showAggrOp AggrMax            = "MAX"
showAggrOp AggrStdDev         = "StdDev"
showAggrOp AggrStdDevP        = "StdDevP"
showAggrOp AggrVar            = "Var"
showAggrOp AggrVarP           = "VarP"
showAggrOp AggrBoolAnd        = "BOOL_AND"
showAggrOp AggrBoolOr         = "BOOL_OR"
showAggrOp AggrArr            = "ARRAY_AGG"
showAggrOp (AggrStringAggr _) = "STRING_AGG"
showAggrOp (AggrOther s)      = s


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
escape c = [c]


-- | Quote binary literals using Postgresql's hex format.
binQuote :: ByteString -> String
binQuote s = "E'\\\\x" ++ BS8.unpack (Base16.encode s) ++ "'"
