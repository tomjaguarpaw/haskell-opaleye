-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

module Opaleye.Internal.HaskellDB.Query where

import Opaleye.Internal.HaskellDB.PrimQuery

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

instance ShowConstant a => ShowConstant (Maybe a) where
    showConstant = maybe NullLit showConstant
