{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.RunQuery where

import           Control.Applicative (Applicative, pure, (<*>))

import           Database.PostgreSQL.Simple.Internal (RowParser)
import           Database.PostgreSQL.Simple.FromField (FieldParser, FromField,
                                                       fromField)
import           Database.PostgreSQL.Simple.FromRow (fieldWith)

import           Opaleye.Column (Column)
import           Opaleye.Internal.Column (Nullable)
import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Unpackspec as U

import qualified Data.Profunctor as P
import           Data.Profunctor (dimap)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product (empty, (***!))
import qualified Data.Profunctor.Product.Default as D

import           GHC.Int (Int64)

data QueryRunnerColumn coltype haskell =
  QueryRunnerColumn (U.Unpackspec (Column coltype) ()) (FieldParser haskell)

data QueryRunner columns haskells = QueryRunner (U.Unpackspec columns ())
                                                (RowParser haskells)

fieldQueryRunnerColumn :: FromField haskell => QueryRunnerColumn coltype haskell
fieldQueryRunnerColumn =
  QueryRunnerColumn (P.rmap (const ()) U.unpackspecColumn) fromField

queryRunner :: QueryRunnerColumn a b -> QueryRunner (Column a) b
queryRunner qrc = QueryRunner u (fieldWith fp)
    where QueryRunnerColumn u fp = qrc

queryRunnerColumnNullable :: QueryRunnerColumn a b
                       -> QueryRunnerColumn (Nullable a) (Maybe b)
queryRunnerColumnNullable qr =
  QueryRunnerColumn (P.lmap C.unsafeCoerce u) (fromField' fp)
  where QueryRunnerColumn u fp = qr
        fromField' :: FieldParser a -> FieldParser (Maybe a)
        fromField' _ _ Nothing = pure Nothing
        fromField' fp' f bs = fmap Just (fp' f bs)

instance D.Default QueryRunnerColumn a b =>
         D.Default QueryRunnerColumn (Nullable a) (Maybe b) where
  def = queryRunnerColumnNullable D.def

instance D.Default QueryRunnerColumn a b =>
         D.Default QueryRunner (Column a) b where
  def = queryRunner D.def

instance D.Default QueryRunnerColumn Int Int where
  def = fieldQueryRunnerColumn

instance D.Default QueryRunnerColumn Int64 Int64 where
  def = fieldQueryRunnerColumn

instance D.Default QueryRunnerColumn Integer Integer where
  def = fieldQueryRunnerColumn

instance D.Default QueryRunnerColumn String String where
  def = fieldQueryRunnerColumn

instance D.Default QueryRunnerColumn Double Double where
  def = fieldQueryRunnerColumn

-- Boilerplate instances

instance Functor (QueryRunner c) where
  fmap f (QueryRunner u r) = QueryRunner u (fmap f r)

-- TODO: Seems like this one should be simpler!
instance Applicative (QueryRunner c) where
  pure = QueryRunner (P.lmap (const ()) PP.empty) . pure
  QueryRunner uf rf <*> QueryRunner ux rx =
    QueryRunner (P.dimap (\x -> (x,x)) (const ()) (uf PP.***! ux)) (rf <*> rx)

instance P.Profunctor QueryRunner where
  dimap f g (QueryRunner u r) = QueryRunner (P.lmap f u) (fmap g r)

instance PP.ProductProfunctor QueryRunner where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

-- }
