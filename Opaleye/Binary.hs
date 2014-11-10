{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Opaleye.Binary where

import           Opaleye.QueryArr (Query)
import qualified Opaleye.QueryArr as Q
import           Opaleye.Column (Column(Column))
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM

import qualified Database.HaskellDB.PrimQuery as HPQ

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))
import           Control.Arrow ((***))

unionAll :: Default Binaryspec columns columns =>
            Query columns -> Query columns -> Query columns
unionAll = unionAllExplicit def

unionAllExplicit :: Binaryspec columns columns'
                 -> Query columns -> Query columns -> Query columns'
unionAllExplicit binaryspec q1 q2 = Q.simpleQueryArr q where
  q ((), startTag) = (newColumns, newPrimQuery, T.next endTag)
    where (columns1, primQuery1, midTag) = Q.runSimpleQueryArr q1 ((), startTag)
          (columns2, primQuery2, endTag) = Q.runSimpleQueryArr q2 ((), midTag)

          (newColumns, pes) =
            PM.run (runBinaryspec binaryspec extractBinaryFields
                                  (columns1, columns2))

          newPrimQuery = PQ.Binary PQ.UnionAll pes (primQuery1, primQuery2)

extractBinaryFields :: (HPQ.PrimExpr, HPQ.PrimExpr)
                    -> PM.PM [(String, (HPQ.PrimExpr, HPQ.PrimExpr))]
                             HPQ.PrimExpr
extractBinaryFields pes = do
  i <- PM.new
  let s = "binary" ++ i
  PM.write (s, pes)
  return (HPQ.AttrExpr s)

data Binaryspec columns columns' =
  Binaryspec (PM.PackMap (HPQ.PrimExpr, HPQ.PrimExpr) HPQ.PrimExpr
                         (columns, columns) columns')

runBinaryspec :: Applicative f => Binaryspec columns columns'
                 -> ((HPQ.PrimExpr, HPQ.PrimExpr) -> f HPQ.PrimExpr)
                 -> (columns, columns) -> f columns'
runBinaryspec (Binaryspec b) f = PM.packmap b f

instance Default Binaryspec (Column a) (Column a) where
  def = Binaryspec (PM.PackMap (\f (Column e, Column e')
                                -> fmap Column (f (e, e'))))

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
