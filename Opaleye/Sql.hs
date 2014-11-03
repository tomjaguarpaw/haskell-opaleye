module Opaleye.Sql where

import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Database.HaskellDB.Sql.Print as P
import qualified Database.HaskellDB.Sql.Generate as G
import qualified Database.HaskellDB.Sql.Default as D
import qualified Database.HaskellDB.Optimize as O

import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.QueryArr as Q

import           Karamaan.Plankton ((.:))

showSqlForPostgresExplicit :: U.Unpackspec columns b -> Q.Query columns -> String
showSqlForPostgresExplicit = formatAndShowSQL . O.optimize .: Q.runQueryArrUnpack

formatAndShowSQL :: PQ.PrimQuery -> String
formatAndShowSQL = show . P.ppSql . G.sqlQuery D.defaultSqlGenerator
