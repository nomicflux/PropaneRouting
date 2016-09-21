module App where

import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Servant (ServantErr)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource)
import System.Environment (lookupEnv)
import System.Log.FastLogger (LoggerSet, LogStr, pushLogStrLn, toLogStr)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

type HubID = Int64

type TankID = Int64
type ReadingID = Int64
type Reading = Int
type Lat = Double
type Lng = Double
data Position = Position Lat Lng

type DBPool = Pool Connection

data Environment = Test
                 | Development
                 | Production
  deriving (Show, Eq, Read)

data LogTo = STDOut
           | STDErr
           | File String
  deriving (Show, Eq, Read)

data Config = Config
              { getPool :: DBPool
              , getLogger :: LoggerSet
              }

type AppM = ReaderT Config (ExceptT ServantErr IO)

getConnFromPool :: DBPool -> AppM Connection
getConnFromPool pool = withResource pool return

getConn :: AppM Connection
getConn = ask >>= getConnFromPool . getPool

addToLogger :: String -> AppM ()
addToLogger msg = ask >>= \cfg -> liftIO $ pushLogStrLn (getLogger cfg) $ toLogStr msg

lookupEnvDefault :: Read a => String -> a -> IO a
lookupEnvDefault var def = do
  env <- lookupEnv var
  return (fromMaybe def $ env >>= readMaybe)
