-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.PrimQuery where

import qualified Opaleye.Internal.Tag as T
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NEL

type TableName  = String
type Attribute  = String
type Name = String
type Scheme     = [Attribute]
type Assoc      = [(Attribute,PrimExpr)]

data Symbol = Symbol String T.Tag deriving (Read, Show)

data PrimExpr   = AttrExpr  Symbol
                | BaseTableAttrExpr Attribute
                | CompositeExpr     PrimExpr Attribute -- ^ Composite Type Query
                | BinExpr   BinOp PrimExpr PrimExpr
                | UnExpr    UnOp PrimExpr
                | AggrExpr  AggrDistinct AggrOp PrimExpr [OrderExpr]
                | ConstExpr Literal
                | CaseExpr [(PrimExpr,PrimExpr)] PrimExpr
                | ListExpr (NEL.NonEmpty PrimExpr)
                | ParamExpr (Maybe Name) PrimExpr
                | FunExpr Name [PrimExpr]
                | CastExpr Name PrimExpr -- ^ Cast an expression to a given type.
                | DefaultInsertExpr -- Indicate that we want to insert the
                                    -- default value into a column.
                                    -- TODO: I'm not sure this belongs
                                    -- here.  Perhaps a special type is
                                    -- needed for insert expressions.
                | ArrayExpr [PrimExpr] -- ^ ARRAY[..]
                | RangeExpr BoundExpr BoundExpr
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

data BinOp      = (:==) | (:<) | (:<=) | (:>) | (:>=) | (:<>)
                | OpAnd | OpOr
                | OpLike | OpILike | OpIn
                | OpOther String

                | (:||)
                | (:+) | (:-) | (:*) | (:/) | OpMod
                | (:~) | (:&) | (:|) | (:^)
                | (:=) | OpAtTimeZone

                | (:->) | (:->>) | (:#>) | (:#>>)
                | (:@>) | (:<@) | (:?) | (:?|) | (:?&)
                | (:&&) | (:<<) | (:>>) | (:&<) | (:&>) | (:-|-)
                | OpArrayIndex
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

data AggrDistinct = AggrDistinct | AggrAll
                  deriving (Eq,Show,Read)

data OrderExpr = OrderExpr OrderOp PrimExpr
               deriving (Show,Read)

data OrderNulls = NullsFirst | NullsLast
                deriving (Show,Read)

data OrderDirection = OpAsc | OpDesc
                    deriving (Show,Read)

data OrderOp = OrderOp { orderDirection :: OrderDirection
                       , orderNulls     :: OrderNulls }
               deriving (Show,Read)

data BoundExpr = Inclusive PrimExpr | Exclusive PrimExpr | PosInfinity | NegInfinity
                 deriving (Show,Read)
