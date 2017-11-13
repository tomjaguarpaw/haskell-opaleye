{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Internal.Binary where

import           Opaleye.Internal.Column (Column(Column), unColumn)
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.QueryArr as Q
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))
import           Control.Arrow ((***))

extractBinaryFields :: T.Tag -> (HPQ.PrimExpr, HPQ.PrimExpr)
                    -> PM.PM [(HPQ.Symbol, (HPQ.PrimExpr, HPQ.PrimExpr))]
                             HPQ.PrimExpr
extractBinaryFields = PM.extractAttr "binary"

newtype Binaryspec columns columns' =
  Binaryspec (PM.PackMap (HPQ.PrimExpr, HPQ.PrimExpr) HPQ.PrimExpr
                         (columns, columns) columns')

runBinaryspec :: Applicative f => Binaryspec columns columns'
                 -> ((HPQ.PrimExpr, HPQ.PrimExpr) -> f HPQ.PrimExpr)
                 -> (columns, columns) -> f columns'
runBinaryspec (Binaryspec b) = PM.traversePM b

binaryspecColumn :: Binaryspec (Column a) (Column a)
binaryspecColumn = Binaryspec (PM.iso (mapBoth unColumn) Column)
  where mapBoth f (s, t) = (f s, f t)

sameTypeBinOpHelper :: PQ.BinOp -> Binaryspec columns columns'
                    -> Q.Query columns -> Q.Query columns -> Q.Query columns'
sameTypeBinOpHelper binop binaryspec q1 q2 = Q.simpleQueryArr q where
  q ((), startTag) = (newColumns, newPrimQuery, T.next endTag)
    where (columns1, primQuery1, midTag) = Q.runSimpleQueryArr q1 ((), startTag)
          (columns2, primQuery2, endTag) = Q.runSimpleQueryArr q2 ((), midTag)

          (newColumns, pes) =
            PM.run (runBinaryspec binaryspec (extractBinaryFields endTag)
                                    (columns1, columns2))

          newPrimQuery = PQ.Binary binop pes (primQuery1, primQuery2)

instance Default Binaryspec (Column a) (Column a) where
  def = binaryspecColumn

-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (Binaryspec a) where
  fmap f (Binaryspec g) = Binaryspec (fmap f g)

instance Applicative (Binaryspec a) where
  pure = Binaryspec . pure
  Binaryspec f <*> Binaryspec x = Binaryspec (f <*> x)

instance Profunctor Binaryspec where
  dimap f g (Binaryspec b) = Binaryspec (dimap (f *** f) g b)

instance ProductProfunctor Binaryspec where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

-- }
