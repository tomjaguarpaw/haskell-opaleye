-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.Sql ( 
                               SqlTable,
                               SqlColumn,
                               SqlName,
                               SqlOrder(..),

	                       SqlUpdate(..), 
	                       SqlDelete(..), 
	                       SqlInsert(..), 

                               SqlExpr(..),
                               Mark(..),
	                      ) where


-----------------------------------------------------------
-- * SQL data type
-----------------------------------------------------------

type SqlTable = String

type SqlColumn = String

-- | A valid SQL name for a parameter.
type SqlName = String

data SqlOrder = SqlAsc | SqlDesc
  deriving Show

data Mark = Columns [(SqlColumn, SqlExpr)]
  deriving Show


-- | Expressions in SQL statements.
data SqlExpr = ColumnSqlExpr  SqlColumn
             | BinSqlExpr     String SqlExpr SqlExpr
             | PrefixSqlExpr  String SqlExpr
             | PostfixSqlExpr String SqlExpr
             | FunSqlExpr     String [SqlExpr]
             | AggrFunSqlExpr String [SqlExpr] -- ^ Aggregate functions separate from normal functions.
             | ConstSqlExpr   String
	     | CaseSqlExpr    [(SqlExpr,SqlExpr)] SqlExpr
             | ListSqlExpr    [SqlExpr]
             | ParamSqlExpr (Maybe SqlName) SqlExpr
             | PlaceHolderSqlExpr
             | ParensSqlExpr SqlExpr
             | CastSqlExpr String SqlExpr 
  deriving Show

-- | Data type for SQL UPDATE statements.
data SqlUpdate  = SqlUpdate SqlTable [(SqlColumn,SqlExpr)] [SqlExpr]

-- | Data type for SQL DELETE statements.
data SqlDelete  = SqlDelete SqlTable [SqlExpr]

--- | Data type for SQL INSERT statements.
data SqlInsert  = SqlInsert      SqlTable [SqlColumn] [SqlExpr]
