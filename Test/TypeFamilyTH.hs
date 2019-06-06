{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Opaleye.Internal.TypeFamilies
import           Opaleye.Internal.TypeFamiliesTH
import           Opaleye.SqlTypes


data UserP f = UserP
    { pName      :: TableField f String SqlText NN Req
    , pUserEmail :: TableField f String SqlText NN Req
    , pEnabled   :: TableField f Bool   SqlBool NN Opt
    }

makeTableAdaptorInstanceMap "pUser" ''UserP


data ResultP f = Result
    { rName    :: RecordField f String SqlText NN
    , rMessage :: RecordField f String SqlText NN
    }

makeTableAdaptorInstanceMap "pResult" ''ResultP


-- If this compiles, this works
main :: IO ()
main = return ()
