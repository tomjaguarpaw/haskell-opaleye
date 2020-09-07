module Opaleye.Experimental.Enum
  (
    fromFieldToFieldsEnum
  ) where

import           Opaleye.Column (Column)
import qualified Opaleye as O
import qualified Opaleye.Internal.RunQuery as RQ

import           Data.ByteString.Char8 (unpack)

fromFieldToFieldsEnum :: String
                      -> (String -> Maybe haskellSum)
                      -> (haskellSum -> String)
                      -> (RQ.FromField sqlEnum haskellSum,
                          O.ToFields haskellSum (Column sqlEnum))
fromFieldToFieldsEnum type_ from to_ = (fromFieldEnum, toFieldsEnum)
   where
     toFieldsEnum = O.toToFields (O.unsafeCast type_ . O.sqlString . to_)
     fromFieldEnum = flip fmap RQ.unsafeFromFieldRaw $ \(_, mdata) -> case mdata of
       Nothing -> error "Unexpected NULL"
       Just s -> case from (unpack s) of
         Just r -> r
         Nothing -> error ("Unexpected: " ++ unpack s)
