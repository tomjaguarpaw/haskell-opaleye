-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.PrimQuery where

import qualified Opaleye.Internal.Tag as T
import Data.ByteString (ByteString)

type TableName  = String
type Attribute  = String
type Name = String
type Scheme     = [Attribute]
type Assoc      = [(Attribute,PrimExpr)]

data Symbol = Symbol String T.Tag deriving (Read, Show)

data PrimExpr   = AttrExpr  Symbol
                | BaseTableAttrExpr Attribute
                | BinExpr   BinOp PrimExpr PrimExpr
                | UnExpr    UnOp PrimExpr
                | AggrExpr  AggrOp PrimExpr
                | ConstExpr Literal
                | CaseExpr [(PrimExpr,PrimExpr)] PrimExpr
                | ListExpr [PrimExpr]
                | ParamExpr (Maybe Name) PrimExpr
                | FunExpr Name [PrimExpr]
                | CastExpr Name PrimExpr -- ^ Cast an expression to a given type.
                | DefaultInsertExpr -- Indicate that we want to insert the
                                    -- default value into a column.
                                    -- TODO: I'm not sure this belongs
                                    -- here.  Perhaps a special type is
                                    -- needed for insert expressions.
                deriving (Read,Show)

data Literal = NullLit
             | DefaultLit            -- ^ represents a default value
             | BoolLit Bool
             | StringLit String
             | ByteStringLit ByteString
             | IntegerLit Integer
             | DoubleLit Double
             | OtherLit String       -- ^ used for hacking in custom SQL
               deriving (Read,Show)

data BinOp      = OpEq | OpLt | OpLtEq | OpGt | OpGtEq | OpNotEq
                | OpAnd | OpOr
                | OpLike | OpIn
                | OpOther String

                | OpCat
                | OpPlus | OpMinus | OpMul | OpDiv | OpMod
                | OpBitNot | OpBitAnd | OpBitOr | OpBitXor
                | OpAsg
                deriving (Show,Read)

data UnOp = OpNot
          | OpIsNull
          | OpIsNotNull
          | OpLength
          | OpAbs
          | OpNegate
          | OpLower
          | OpUpper
          | UnOpOther String
          deriving (Show,Read)

data AggrOp     = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
                | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
                | AggrBoolOr | AggrBoolAnd | AggrArr | AggrStringAggr PrimExpr
                | AggrOther String
                deriving (Show,Read)

data OrderExpr = OrderExpr OrderOp PrimExpr
               deriving (Show)

data OrderNulls = NullsFirst | NullsLast
                deriving Show

data OrderDirection = OpAsc | OpDesc
                    deriving Show

data OrderOp = OrderOp { orderDirection :: OrderDirection
                       , orderNulls     :: OrderNulls }
               deriving (Show)
