-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

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
                | AggrExpr  (Aggr' PrimExpr)
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
                deriving (Read,Show)

traverseSymbols :: Applicative f => (Symbol -> f Symbol) -> PrimExpr -> f PrimExpr
traverseSymbols f = go
  where
    go = \case
      AttrExpr symbol -> AttrExpr <$> f symbol
      BaseTableAttrExpr attribute -> pure $ BaseTableAttrExpr attribute
      CompositeExpr a attribute -> CompositeExpr <$> go a <*> pure attribute
      BinExpr op a b -> BinExpr op <$> go a <*> go b
      UnExpr op a -> UnExpr op <$> go a
      AggrExpr aggr -> AggrExpr <$> traverse go aggr
      WndwExpr wndw partition -> WndwExpr <$> traverse go wndw <*> traverse go partition
      ConstExpr literal -> pure $ ConstExpr literal
      CaseExpr conds a -> CaseExpr <$> traverse (bitraverse go go) conds <*> go a
      ListExpr as -> ListExpr <$> traverse go as
      ParamExpr name a -> ParamExpr name <$> go a
      FunExpr name args -> FunExpr name <$> traverse go args
      CastExpr name a -> CastExpr name <$> go a
      DefaultInsertExpr -> pure DefaultInsertExpr
      ArrayExpr as -> ArrayExpr <$> traverse go as
      RangeExpr s a b -> RangeExpr s <$> traverse go a <*> traverse go b
      ArrayIndex a b -> ArrayIndex <$> go a <*> go b
    bitraverse g h (a, b) = (,) <$> g a <*> h b

data Literal = NullLit
             | DefaultLit            -- ^ represents a default value
             | BoolLit Bool
             | StringLit String
             | ByteStringLit ByteString
             | IntegerLit Integer
             | DoubleLit Double
             | NumericLit Sci.Scientific
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
                | AggrBoolOr | AggrBoolAnd | AggrArr | JsonArr
                | AggrStringAggr
                | AggrOther String
                deriving (Show,Read)

data AggrDistinct = AggrDistinct | AggrAll
                  deriving (Eq,Show,Read)

type Aggregate = Aggregate' PrimExpr

data Aggregate' a = GroupBy a | Aggregate (Aggr' a)
  deriving (Functor, Foldable, Traversable, Show, Read)

data Aggr' a = Aggr
  { aggrOp :: !AggrOp
  , aggrExprs :: ![a]
  , aggrOrder :: ![OrderExpr' a]
  , aggrDistinct :: !AggrDistinct
  , aggrGroup :: ![OrderExpr' a]
  , aggrFilter :: !(Maybe PrimExpr)
  }
  deriving (Functor, Foldable, Traversable, Show, Read)

type OrderExpr = OrderExpr' PrimExpr

data OrderExpr' a = OrderExpr OrderOp a
  deriving (Functor, Foldable, Traversable, Show, Read)

data OrderNulls = NullsFirst | NullsLast
                deriving (Show,Read)

data OrderDirection = OpAsc | OpDesc
                    deriving (Show,Read)

data OrderOp = OrderOp { orderDirection :: OrderDirection
                       , orderNulls     :: OrderNulls }
               deriving (Show,Read)

type BoundExpr = BoundExpr' PrimExpr

data BoundExpr' a = Inclusive a | Exclusive a | PosInfinity | NegInfinity
  deriving (Foldable, Functor, Traversable, Read, Show)

type WndwOp = WndwOp' PrimExpr

data WndwOp' a
  = WndwRowNumber
  | WndwRank
  | WndwDenseRank
  | WndwPercentRank
  | WndwCumeDist
  | WndwNtile a
  | WndwLag a a a
  | WndwLead a a a
  | WndwFirstValue a
  | WndwLastValue a
  | WndwNthValue a a
  | WndwAggregate AggrOp [a]
  deriving (Foldable, Functor, Traversable, Show, Read)

type Partition = Partition' PrimExpr

data Partition' a = Partition
  { partitionBy :: [a]
  , orderBy :: [OrderExpr' a]
  }
  deriving (Foldable, Functor, Traversable, Read, Show)
