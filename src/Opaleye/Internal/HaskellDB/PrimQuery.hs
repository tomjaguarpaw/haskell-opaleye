-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

{-# LANGUAGE DeriveTraversable #-}

module Opaleye.Internal.HaskellDB.PrimQuery where

import qualified Opaleye.Internal.Tag as T
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Scientific as Sci

type TableName  = String
type Attribute  = String
type Name = String
type Scheme     = [Attribute]
type Assoc      = [(Attribute,PrimExpr)]

data Symbol = Symbol String T.Tag deriving (Eq, Ord, Read, Show)

data PrimExpr   = AttrExpr  Symbol
                | BaseTableAttrExpr Attribute
                | CompositeExpr     PrimExpr Attribute -- ^ Composite Type Query
                | BinExpr   BinOp PrimExpr PrimExpr
                | UnExpr    UnOp PrimExpr
                | AggrExpr  Aggr
                | WndwExpr  WndwOp Partition
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
                | RangeExpr String BoundExpr BoundExpr
                | ArrayIndex PrimExpr PrimExpr
                deriving (Eq, Ord, Read, Show)

data Literal = NullLit
             | DefaultLit            -- ^ represents a default value
             | BoolLit Bool
             | StringLit String
             | ByteStringLit ByteString
             | IntegerLit Integer
             | DoubleLit Double
             | NumericLit Sci.Scientific
             | OtherLit String       -- ^ used for hacking in custom SQL
               deriving (Eq, Ord, Read, Show)

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
                deriving (Eq, Ord, Read, Show)

data UnOp = OpNot
          | OpIsNull
          | OpIsNotNull
          | OpLength
          | OpAbs
          | OpNegate
          | OpLower
          | OpUpper
          | UnOpOther String
          deriving (Eq, Ord, Read, Show)

data AggrOp     = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
                | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
                | AggrBoolOr | AggrBoolAnd | AggrArr | JsonArr
                | AggrStringAggr
                | AggrOther String
                deriving (Eq, Ord, Read, Show)

data AggrDistinct = AggrDistinct | AggrAll
                  deriving (Eq, Ord, Read, Show)

type Aggregate = Aggregate' PrimExpr

data Aggregate' a = GroupBy a | Aggregate (Aggr' a)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Read, Show)

type Aggr = Aggr' PrimExpr

data Aggr' a = Aggr
  { aggrOp :: !AggrOp
  , aggrExprs :: ![a]
  , aggrOrder :: ![OrderExpr' a]
  , aggrDistinct :: !AggrDistinct
  , aggrFilter :: !(Maybe PrimExpr)
  }
  deriving (Functor, Foldable, Traversable, Eq, Ord, Read, Show)

type OrderExpr = OrderExpr' PrimExpr

data OrderExpr' a = OrderExpr OrderOp a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Read, Show)

data OrderNulls = NullsFirst | NullsLast
                deriving (Eq, Ord, Read, Show)

data OrderDirection = OpAsc | OpDesc
                    deriving (Eq, Ord, Read, Show)

data OrderOp = OrderOp { orderDirection :: OrderDirection
                       , orderNulls     :: OrderNulls }
               deriving (Eq, Ord, Read, Show)

data BoundExpr = Inclusive PrimExpr | Exclusive PrimExpr | PosInfinity | NegInfinity
                 deriving (Eq, Ord, Read, Show)

data WndwOp
  = WndwRowNumber
  | WndwRank
  | WndwDenseRank
  | WndwPercentRank
  | WndwCumeDist
  | WndwNtile PrimExpr
  | WndwLag PrimExpr PrimExpr PrimExpr
  | WndwLead PrimExpr PrimExpr PrimExpr
  | WndwFirstValue PrimExpr
  | WndwLastValue PrimExpr
  | WndwNthValue PrimExpr PrimExpr
  | WndwAggregate AggrOp [PrimExpr]
  deriving (Eq, Ord, Read, Show)

data Partition = Partition
  { partitionBy :: [PrimExpr]
  , orderBy :: [OrderExpr]
  }
  deriving (Eq, Ord, Read, Show)
