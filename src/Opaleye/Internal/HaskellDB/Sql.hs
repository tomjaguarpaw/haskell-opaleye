-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.Sql where


import qualified Data.List.NonEmpty as NEL

-----------------------------------------------------------
-- * SQL data type
-----------------------------------------------------------

data SqlTable = SqlTable
  { sqlTableSchemaName :: Maybe String
  , sqlTableName       :: String
  } deriving Show

newtype SqlColumn = SqlColumn String deriving Show

-- | A valid SQL name for a parameter.
type SqlName = String

data SqlOrderNulls = SqlNullsFirst | SqlNullsLast
                   deriving Show

data SqlOrderDirection = SqlAsc | SqlDesc
                       deriving Show

data SqlOrder = SqlOrder { sqlOrderDirection :: SqlOrderDirection
                         , sqlOrderNulls     :: SqlOrderNulls }
  deriving Show

-- | Expressions in SQL statements.
data SqlExpr = ColumnSqlExpr  SqlColumn
             | CompositeSqlExpr SqlExpr String
             | BinSqlExpr     String SqlExpr SqlExpr
             | PrefixSqlExpr  String SqlExpr
             | PostfixSqlExpr String SqlExpr
             | FunSqlExpr     String [SqlExpr]
             | AggrFunSqlExpr String [SqlExpr] [(SqlExpr, SqlOrder)] -- ^ Aggregate functions separate from normal functions.
             | ConstSqlExpr   String
             | CaseSqlExpr    (NEL.NonEmpty (SqlExpr,SqlExpr)) SqlExpr
             | ListSqlExpr    (NEL.NonEmpty SqlExpr)
             | ParamSqlExpr (Maybe SqlName) SqlExpr
             | PlaceHolderSqlExpr
             | ParensSqlExpr SqlExpr
             | CastSqlExpr String SqlExpr
             | DefaultSqlExpr
             | ArraySqlExpr [SqlExpr]
  deriving Show

-- | Data type for SQL UPDATE statements.
data SqlUpdate  = SqlUpdate SqlTable [(SqlColumn,SqlExpr)] [SqlExpr]

-- | Data type for SQL DELETE statements.
data SqlDelete  = SqlDelete SqlTable [SqlExpr]

--- | Data type for SQL INSERT statements.
data SqlInsert  = SqlInsert SqlTable [SqlColumn] (NEL.NonEmpty [SqlExpr])
