module Connection where

import qualified Database.PostgreSQL.Simple       as PGS

type Connection = PGS.Connection

withConnection :: Connection -> (PGS.Connection -> IO r) -> IO (Either () r)
withConnection conn k = fmap Right (k conn)
