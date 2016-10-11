{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import Data.Int (Int64)
import Data.Yaml ((.:), (.:?), (.!=), parseJSON, FromJSON)
import qualified Data.Yaml as Yaml
import GHC.Generics
-- import Control.Concurrent.STM (TVar)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Servant (ServantErr)
import Database.PostgreSQL.Simple (Connection, execute_)
import Data.Pool (Pool, withResource)
import Data.Text (unpack)
import Data.ByteString (ByteString)
-- import System.Environment (lookupEnv)
import System.Log.FastLogger (LoggerSet, pushLogStrLn, toLogStr)
-- import Data.Maybe (fromMaybe)
-- import Text.Read (readMaybe)

type VendorID = Int64
type HubID = Int64
type TankID = Int64
type ReadingID = Int64
type Reading = Int
type Lat = Double
type Lng = Double
type Username = String
type Password = ByteString
data Position = Position Lat Lng

type DBPool = Pool Connection

data Environment = Test
                 | Development
                 | Production
  deriving (Show, Eq, Read, Generic)

instance FromJSON Environment where

data LogTo = STDOut
           | STDErr
           | File String
  deriving (Show, Eq, Read, Generic)

instance FromJSON LogTo where
  parseJSON (Yaml.String s)
    | s == "STDOut" = return STDOut
    | s == "STDErr" = return STDErr
    | otherwise = return $ File (unpack s)
  parseJSON (Yaml.Object o) = File <$> o .: "file"
  parseJSON _ = mzero

data EnvConfig = EnvConfig { envPort :: Maybe Int
                           , envEnvironment :: Maybe Environment
                           , envLogTo :: Maybe LogTo
                           , envCert :: String
                           , envChain :: [String]
                           , envKey :: String
                           , envPGUser :: String
                           , envPGPassword :: String
                           , envPGDatabase :: String
                           }
  deriving (Show, Eq)

instance FromJSON EnvConfig where
  parseJSON (Yaml.Object o) = EnvConfig <$>
    o .:? "port" <*>
    o .:? "env" <*>
    o .:? "log_to" <*>
    o .: "cert_file" <*>
    (o .:? "chain_file" .!= []) <*>
    o .: "key_file" <*>
    o .: "pg_user" <*>
    o .: "pg_pwd" <*>
    o .: "pg_db"
  parseJSON _ = mzero

data Config = Config
              { getPool :: DBPool
              , getLogger :: LoggerSet
              }

mkConfig :: DBPool -> LoggerSet -> IO Config
mkConfig pool logger =
  return $ Config pool logger

type AppM = ReaderT Config (ExceptT ServantErr IO)
type ConfigT = ReaderT Config

getConnFromPool :: MonadBaseControl IO m => DBPool -> m Connection
getConnFromPool pool = withResource pool return

getConn :: MonadBaseControl IO m => ConfigT m Connection
getConn = ask >>= getConnFromPool . getPool

listenToNotifications :: (MonadIO m, MonadBaseControl IO m) => ConfigT m Bool
listenToNotifications = do
  con <- getConn
  res <- liftIO $ execute_ con "LISTEN addedreading"
  if res == 0
    then return False
    else return True

unlistenToNotifications :: (MonadIO m, MonadBaseControl IO m) => ConfigT m Bool
unlistenToNotifications = do
  con <- getConn
  res <- liftIO $ execute_ con "UNLISTEN addedreading"
  if res == 0
    then return False
    else return True

addToLogger :: MonadIO m => String -> ConfigT m ()
addToLogger msg = ask >>= \cfg -> liftIO $ pushLogStrLn (getLogger cfg) $ toLogStr msg

-- lookupEnvDefault :: Read a => String -> a -> IO a
-- lookupEnvDefault var def = do
--   env <- lookupEnv var
--   return (fromMaybe def $ env >>= readMaybe)
