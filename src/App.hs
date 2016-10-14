{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import Data.Int (Int64)
import Data.Yaml ((.:), (.:?), (.!=), parseJSON, FromJSON)
import qualified Data.Yaml as Yaml
import GHC.Generics
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Servant (ServantErr)
import Database.PostgreSQL.Simple (Connection, execute_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Pool (Pool, withResource)
import Data.Text (unpack, Text)
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
-- import System.Environment (lookupEnv)
import System.Log.FastLogger (LoggerSet, pushLogStrLn, toLogStr)
-- import Data.Maybe (fromMaybe)
-- import Text.Read (readMaybe)
import Web.JWT (Secret, binarySecret)

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
                           , envSecret :: ByteString
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
    o .: "pg_db" <*>
    (BS.pack <$> (o .: "secret"))
  parseJSON _ = mzero

data Config = Config
              { getPool :: DBPool
              , getLogger :: LoggerSet
              , getSession :: TVar (IntMap Text)
              , getSecret :: Secret
              }

mkConfig :: DBPool -> LoggerSet -> ByteString -> IO Config
mkConfig pool logger bssecret = do
  session <- STM.newTVarIO IM.empty
  return $ Config pool logger session (binarySecret bssecret)

addVendor :: TVar (IntMap Text) -> VendorID -> Text -> STM.STM ()
addVendor session vid token = STM.modifyTVar session (IM.insert (fromIntegral vid) token)

removeVendor :: TVar (IntMap Text) -> VendorID -> STM.STM ()
removeVendor session vid = STM.modifyTVar session (IM.delete (fromIntegral vid))

checkVendor :: TVar (IntMap Text) -> VendorID -> Text -> STM.STM Bool
checkVendor session vid attempt = do
  vendors <- STM.readTVar session
  let mchecks = (== attempt) <$> IM.lookup (fromIntegral vid) vendors
  case mchecks of
    Just True -> return True
    _ -> return False

addToSession :: (MonadIO m, MonadBaseControl IO m) => VendorID -> Text -> ConfigT m ()
addToSession vid token = do
  session <- getSession <$> ask
  liftIO $ STM.atomically $ addVendor session vid token

removeFromSession :: (MonadIO m, MonadBaseControl IO m) => VendorID -> ConfigT m ()
removeFromSession vid = do
  session <- getSession <$> ask
  liftIO $ STM.atomically $ removeVendor session vid

verifyToken :: (MonadIO m, MonadBaseControl IO m) => VendorID -> Text -> ConfigT m Bool
verifyToken vid token = do
  session <- getSession <$> ask
  liftIO . STM.atomically $ checkVendor session vid token


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
