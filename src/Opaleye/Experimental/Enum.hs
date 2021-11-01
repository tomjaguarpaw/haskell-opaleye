{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Opaleye.Experimental.Enum
  (
    enumMapper,
    EnumMapper,
    enumMapperWithSchema,
    enumFromField,
    enumToFields,
    fromFieldToFieldsEnum,
  ) where

import           Opaleye.Column (Column)
import qualified Opaleye as O
import qualified Opaleye.Internal.Inferrable as I
import qualified Opaleye.Internal.RunQuery as RQ

import           Data.ByteString.Char8 (unpack)
import qualified Data.Profunctor.Product.Default as D
import Text.PrettyPrint.HughesPJ ((<>), doubleQuotes, render, text)
import Prelude hiding ((<>))

data EnumMapper sqlEnum haskellSum = EnumMapper {
    enumFromField :: RQ.FromField sqlEnum haskellSum
  , enumToFields :: O.ToFields haskellSum (Column sqlEnum)
  }

-- | Create a mapping between a Postgres @ENUM@ type and a Haskell
-- type.  Also works for @DOMAIN@ types. For example, if you have the
-- following @ENUM@
--
-- @
-- CREATE TYPE public.mpaa_rating AS ENUM (
--    \'G\',
--    \'PG\',
--    \'PG-13\',
--    \'R\',
--    \'NC-17\'
-- );
-- @
--
-- then you can define data types to represent the enum on the SQL
-- side and Haskell side respectively
--
-- @
-- data SqlRating
-- data Rating = G | PG | PG13 | R | NC17 deriving Show
-- @
--
-- and functions to map between them
--
-- @
-- toSqlRatingString :: Rating -> String
-- toSqlRatingString r = case r of
--     G    -> \"G\"
--     PG   -> \"PG\"
--     PG13 -> \"PG-13\"
--     R    -> \"R\"
--     NC17 -> \"NC-17\"
--
-- fromSqlRatingString :: String -> Maybe Rating
-- fromSqlRatingString s = case s of
--     \"G\"     -> Just G
--     \"PG\"    -> Just PG
--     \"PG-13\" -> Just PG13
--     \"R\"     -> Just R
--     \"NC-17\" -> Just NC17
--     _       -> Nothing
-- @
--
-- Then you can use the mappings as follows
--
-- @
-- import qualified Opaleye as O
-- import qualified Data.Profunctor.Product.Default as D
--
-- sqlRatingMapper :: EnumMapper SqlRating Rating
-- sqlRatingMapper = enumMapper "mpaa_rating" fromSqlRatingString toSqlRatingString
--
-- instance O.DefaultFromField SqlRating Rating where
--   defaultFromField = enumFromField sqlRatingMapper
--
-- instance rating ~ Rating
--   => D.Default (Inferrable O.FromFields) (O.Column SqlRating) rating where
--   def = Inferrable D.def
--
-- instance D.Default O.ToFields Rating (O.Column SqlRating) where
--   def = enumToFields sqlRatingMapper
-- @
enumMapper :: String
           -- ^ The name of the @ENUM@ type
           -> (String -> Maybe haskellSum)
           -- ^ A function which converts from the string
           -- representation of the ENUM field
           -> (haskellSum -> String)
           -- ^ A function which converts to the string representation
           -- of the ENUM field
           -> EnumMapper sqlEnum haskellSum
           -- ^ The @sqlEnum@ type variable is phantom. To protect
           -- yourself against type mismatches you should set it to
           -- the Haskell type that you use to represent the @ENUM@.
enumMapper type_ = enumMapper' (render (doubleQuotes (text type_)))

enumMapperWithSchema :: String
           -- ^ The schema of the @ENUM@ type
           -> String
           -- ^ The name of the @ENUM@ type
           -> (String -> Maybe haskellSum)
           -- ^ A function which converts from the string
           -- representation of the ENUM field
           -> (haskellSum -> String)
           -- ^ A function which converts to the string representation
           -- of the ENUM field
           -> EnumMapper sqlEnum haskellSum
           -- ^ The @sqlEnum@ type variable is phantom. To protect
           -- yourself against type mismatches you should set it to
           -- the Haskell type that you use to represent the @ENUM@.
enumMapperWithSchema schema type_ = enumMapper' (render (doubleQuotes (text schema) <> text "." <> doubleQuotes (text type_)))

enumMapper' :: String
           -- ^ The name of the @ENUM@ type
           -> (String -> Maybe haskellSum)
           -- ^ A function which converts from the string
           -- representation of the ENUM field
           -> (haskellSum -> String)
           -- ^ A function which converts to the string representation
           -- of the ENUM field
           -> EnumMapper sqlEnum haskellSum
           -- ^ The @sqlEnum@ type variable is phantom. To protect
           -- yourself against type mismatches you should set it to
           -- the Haskell type that you use to represent the @ENUM@.
enumMapper' type_ from to_ = EnumMapper {
    enumFromField = fromFieldEnum
  , enumToFields = toFieldsEnum
  }
   where
     toFieldsEnum = O.toToFields (O.unsafeCast type_ . O.sqlString . to_)
     fromFieldEnum = flip fmap RQ.unsafeFromFieldRaw $ \(_, mdata) -> case mdata of
       Nothing -> error "Unexpected NULL"
       Just s -> case from (unpack s) of
         Just r -> r
         Nothing -> error ("Unexpected: " ++ unpack s)

{-# DEPRECATED fromFieldToFieldsEnum "Use 'enumMapper' instead.  Will be removed in 0.9." #-}
fromFieldToFieldsEnum :: String
                      -> (String -> Maybe haskellSum)
                      -> (haskellSum -> String)
                      -> (RQ.FromField sqlEnum haskellSum,
                          O.ToFields haskellSum (Column sqlEnum))
fromFieldToFieldsEnum type_ from to_ = (enumFromField e, enumToFields e)
  where e = enumMapper type_ from to_
