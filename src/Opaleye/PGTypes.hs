module Opaleye.PGTypes where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.Column as C

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified System.Locale as SL

literalColumn :: HPQ.Literal -> Column a
literalColumn = Column . HPQ.ConstExpr

pgString :: String -> Column String
pgString = literalColumn . HPQ.StringLit

pgText :: Text.Text -> Column String
pgText = literalColumn . HPQ.StringLit . Text.unpack

pgInt :: Int -> Column Int
pgInt = literalColumn . HPQ.IntegerLit . fromIntegral

pgInteger :: Integer -> Column Integer
pgInteger = literalColumn . HPQ.IntegerLit

pgDouble :: Double -> Column Double
pgDouble = literalColumn . HPQ.DoubleLit

pgBool :: Bool -> Column Bool
pgBool = literalColumn . HPQ.BoolLit

pgUUID :: UUID.UUID -> Column UUID.UUID
pgUUID = C.unsafeCoerce . pgString . UUID.toString

pgDay :: Time.Day -> Column Time.Day
pgDay = Column
        . HPQ.CastExpr "date"
        . HPQ.ConstExpr
        . HPQ.OtherLit
        . format
  where formatString = "'%Y-%m-%d'"
        format = Time.formatTime SL.defaultTimeLocale formatString

pgUTCTime :: Time.UTCTime -> Column Time.UTCTime
pgUTCTime = Column
            . HPQ.CastExpr "timestamp"
            . HPQ.ConstExpr
            . HPQ.OtherLit
            . format
  where formatString = "'%Y-%m-%dT%H:%M:%SZ'"
        format = Time.formatTime SL.defaultTimeLocale formatString
