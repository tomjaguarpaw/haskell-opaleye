-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.PrimQuery where

type TableName  = String
type Attribute  = String
type Name = String
type Scheme     = [Attribute]
type Assoc      = [(Attribute,PrimExpr)]


data PrimExpr   = AttrExpr  Attribute
                | BinExpr   BinOp PrimExpr PrimExpr
                | UnExpr    UnOp PrimExpr
                | AggrExpr  AggrOp PrimExpr
                | ConstExpr Literal
		| CaseExpr [(PrimExpr,PrimExpr)] PrimExpr
                | ListExpr [PrimExpr]
                | ParamExpr (Maybe Name) PrimExpr
                | FunExpr Name [PrimExpr]
                | CastExpr Name PrimExpr -- ^ Cast an expression to a given type.
                deriving (Read,Show)

data Literal = NullLit
	     | DefaultLit            -- ^ represents a default value
	     | BoolLit Bool
	     | StringLit String
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

data UnOp	= OpNot 
		| OpIsNull | OpIsNotNull
		| OpLength
		| UnOpOther String
		deriving (Show,Read)

data AggrOp     = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
                | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
                | AggrOther String
                deriving (Show,Read)

data OrderExpr = OrderExpr OrderOp PrimExpr 
               deriving (Show)

data OrderOp = OpAsc | OpDesc
               deriving (Show)
