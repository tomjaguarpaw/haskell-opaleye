module Connection where

import           Control.Concurrent (threadDelay)
import           Control.Exception (tryJust)
import           Data.IORef (IORef, readIORef, writeIORef, newIORef)
import           Data.ByteString (ByteString)
import qualified Database.PostgreSQL.Simple       as PGS
import           GHC.IO.Exception (ioe_description)

type Connection = (IORef PGS.Connection, ByteString)

withConnection :: Connection -> (PGS.Connection -> IO r) -> IO (Either () r)
withConnection (conn, connectString) k = do
  conn' <- readIORef conn

  er <- tryJust (\e -> if ioe_description e == "failed to fetch file descriptor"
                       then Just e
                       else Nothing)
                (k conn')

  case er of
    Right r -> pure (Right r)
    Left _ -> do
      PGS.close conn'
      -- If we reconnect immediately then the connection fails with
      -- "Exception: libpq: failed (FATAL: the database system is in
      -- recovery mode".  We could try to handle that, but it's easier
      -- just to delay for ten seconds, which seems to be enough time
      -- for the database to recover.
      threadDelay (10 * 1000 * 1000) -- microseconds
      conn'new <- PGS.connectPostgreSQL connectString
      writeIORef conn conn'new
      return (Left ())

connectPostgreSQL :: ByteString -> IO Connection
connectPostgreSQL connectString = do
  conn' <- PGS.connectPostgreSQL connectString
  conn <- newIORef conn'
  pure (conn, connectString)

close :: Connection -> IO ()
close (conn, _) = do
  conn' <- readIORef conn
  PGS.close conn'
