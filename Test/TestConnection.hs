module TestConnection where

import qualified Database.PostgreSQL.Simple as PGS
import System.Environment
import qualified Configuration.Dotenv as Dotenv
import qualified Data.String as String
import Control.Applicative ((<|>), pure)
import Prelude

getTestDbConnection :: IO PGS.Connection
getTestDbConnection = do
  let envVarName = "POSTGRES_CONNSTRING"
  connectStringEnvVar <- lookupEnv envVarName
  connectStringDotEnv <- do vars <- Dotenv.parseFile ".env"
                            return (lookup envVarName vars)
                         `Dotenv.onMissingFile`
                         return Nothing
  let connectString = connectStringEnvVar <|> connectStringDotEnv
  conn <- maybe
          (fail ("Set " ++ envVarName ++ " environment variable\n"
                 ++ "For example " ++ envVarName ++ "='user=tom dbname=opaleye_test "
                 ++ "host=localhost port=25433 password=tom'"))
          (PGS.connectPostgreSQL . String.fromString)
          connectString
  pure conn
