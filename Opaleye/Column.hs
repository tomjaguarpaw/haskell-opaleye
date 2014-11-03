module Opaleye.Column where

import qualified Database.HaskellDB.PrimQuery as PQ

newtype Column a = Column PQ.PrimExpr deriving Show
