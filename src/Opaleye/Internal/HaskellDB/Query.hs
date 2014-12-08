-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

module Opaleye.Internal.HaskellDB.Query where

import Opaleye.Internal.HaskellDB.PrimQuery

import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import           Data.Time (UTCTime, formatTime)
import qualified Data.UUID as UUID
import           Data.UUID (UUID)
import           System.Locale (defaultTimeLocale)

class ShowConstant a where
    showConstant :: a -> Literal

instance ShowConstant String where
    showConstant = StringLit
instance ShowConstant Int where
    showConstant = IntegerLit . fromIntegral
instance ShowConstant Integer where
    showConstant = IntegerLit
instance ShowConstant Double where
    showConstant = DoubleLit
instance ShowConstant Bool where
    showConstant = BoolLit
instance ShowConstant UUID where
  showConstant = showConstant . UUID.toString
instance ShowConstant ST.Text where
  showConstant = showConstant . ST.unpack
instance ShowConstant LT.Text where
  showConstant = showConstant . LT.unpack
instance ShowConstant UTCTime where
  showConstant = showConstant . formatTime defaultTimeLocale format
    where
      format = "%Y-%m-%dT%H:%M:%SZ"

instance ShowConstant a => ShowConstant (Maybe a) where
    showConstant = maybe NullLit showConstant
