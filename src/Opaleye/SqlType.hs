module Opaleye.SqlType where

data SqlType = SqlBaseType String
             | SqlArray SqlType
             deriving (Read, Show)
