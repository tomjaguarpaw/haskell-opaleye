-- | Experimental stuff for dynamic typing

{-# LANGUAGE MultiParamTypeClasses #-}

module Opaleye.Internal.Dynamic where

import           Control.Applicative ((<|>))

import qualified Database.PostgreSQL.Simple.FromField as PGS

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.PackMap    as PM
import qualified Opaleye.Internal.RunQuery   as R

import           Data.Profunctor

stringUnpackspec :: U.Unpackspec String String
stringUnpackspec = U.Unpackspec (PM.PackMap (const pure))

stringFromFields :: R.FromFields String String
stringFromFields = R.QueryRunner (lmap const (pure ())) pure (const False)

data SqlDynamic

data Dynamic = String String
             | Int Int
             | Float Float
             | Bool Bool
             deriving Show

instance PGS.FromField Dynamic where
  fromField f mbs = parse String
                    <|> parse Int
                    <|> parse Float
                    <|> parse Bool
    where parse :: PGS.FromField a => (a -> b) -> PGS.Conversion b
          parse g = fmap g (PGS.fromField f mbs)

instance R.QueryRunnerColumnDefault SqlDynamic Dynamic where
  defaultFromField = R.fromPGSFromField
