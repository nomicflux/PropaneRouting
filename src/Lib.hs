{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( startApp
    ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import Servant ((:<|>)( .. ), (:>), (:~>), Context( .. ), BasicAuthCheck)
import qualified Servant as S
import Servant.API.BasicAuth (BasicAuth)
import qualified Servant.API.BasicAuth as SBA
import qualified Opaleye as O
import Data.Monoid ((<>))
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import qualified System.Log.FastLogger as FL
import Data.Default.Class (def)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.ByteString.Char8 as BS
import System.Directory (doesFileExist)
import qualified Data.Yaml as Yaml
import Web.JWT (secret)

import App (Config ( .. )
           , AppM
           , Environment ( .. )
           , LogTo ( .. )
           , VendorID
           , EnvConfig ( .. )
           , mkConfig
           )
import Api.Hub
import Api.Tank
import Api.Reading
import Api.Vendor
import Api.Login
import Notifications.Sockets (appWithSockets)

data ConnectionInfo = ConnectionInfo
                      { connUser :: String
                      , connPassword :: String
                      , connDatabase :: String
                      }

connInfoToPG :: ConnectionInfo -> PGS.ConnectInfo
connInfoToPG connInfo = PGS.defaultConnectInfo
                        { PGS.connectUser = connUser connInfo
                        , PGS.connectPassword = connPassword connInfo
                        , PGS.connectDatabase = connDatabase connInfo
                        }

openConnection :: EnvConfig -> IO PGS.Connection
openConnection cfg = do
  let connInfo = ConnectionInfo { connUser = envPGUser cfg
                                , connPassword = envPGPassword cfg
                                , connDatabase = envPGDatabase cfg
                                }
  con <- PGS.connect (connInfoToPG connInfo)
  _ <- PGS.execute_ con "NOTIFY addedreading"
  return con

makeLogger :: LogTo -> IO FL.LoggerSet
makeLogger logTo = case logTo of
        STDOut -> FL.newStdoutLoggerSet FL.defaultBufSize
        STDErr -> FL.newStderrLoggerSet FL.defaultBufSize
        File filename -> FL.newFileLoggerSet FL.defaultBufSize filename

makeMiddleware :: FL.LoggerSet -> Environment -> IO Wai.Middleware
makeMiddleware logger env = case env of
      Test -> return id
      Production -> MidRL.mkRequestLogger $ def { MidRL.destination = MidRL.Logger logger
                                                , MidRL.outputFormat = MidRL.Apache MidRL.FromSocket
                                                }
      Development -> MidRL.mkRequestLogger $ def { MidRL.destination = MidRL.Logger logger }

startApp :: [String] -> IO ()
startApp args = do
  yamlConfigFile <- Yaml.decodeFileEither "config.yaml"
  yamlConfig <- case yamlConfigFile of
    Left err -> fail (Yaml.prettyPrintParseException err)
    Right cfg -> return cfg
  let port = fromMaybe 8080 (envPort yamlConfig)
      env = fromMaybe Production (envEnvironment yamlConfig)
  doesFileExist (envCert yamlConfig) >>= flip unless (fail "Cert file doesn't exist")
  mapM_ (doesFileExist >=> flip unless (fail $ "Chain file doesn't exist")) (envChain yamlConfig)
  doesFileExist (envKey yamlConfig) >>= flip unless (fail "Key file doesn't exist")
  logTo <- case listToMaybe args of
    Just filename -> return $ File filename
    Nothing -> return $ fromMaybe STDOut (envLogTo yamlConfig)
  pool <- Pool.createPool (openConnection yamlConfig) PGS.close 1 10 5
  logger <- makeLogger logTo
  loggerMidware <- makeMiddleware logger env
  FL.pushLogStrLn logger $ FL.toLogStr $
    "Listening on port " <>
    show port <>
    " at level " <>
    show env <>
    " and logging to "  <>
    show logTo <>
    (if null args then " with no args " else " with args " <> unwords args)
  -- specsToDir [hubSpec, tankSpec, readingSpec] "src/Elm"
  cfg <- mkConfig pool logger
  let tls = Warp.tlsSettingsChain (envCert yamlConfig) (envChain yamlConfig) (envKey yamlConfig)
      settings = Warp.setPort port Warp.defaultSettings
  Warp.runTLS tls settings $ loggerMidware $ appWithSockets cfg $ fullApp cfg

readerTToExcept :: Config -> AppM :~> ExceptT S.ServantErr IO
readerTToExcept pool = S.Nat (`runReaderT` pool)

type APIWeb = "hubs" :> (HubAPIGet :<|> HubAPIPost)
              :<|> "tanks" :> (TankAPIGet :<|> TankAPIPost)
              :<|> "readings" :> ReadingAPIGet

type APISensor = "readings" :> ReadingAPIPost

type AuthAPI = APIWeb :<|> S.Raw

type UnAuthAPI = "vendors" :> VendorAPI
                 :<|> "post" :> APISensor
                 :<|> "login" :> LoginAPI
                 :<|> S.Raw

serverWeb :: VendorID -> S.ServerT APIWeb AppM
serverWeb v = (hubGetServer v :<|> hubPostServer)
              :<|> (tankGetServer v :<|> tankPostServer)
              :<|> readingGetServer v

serverSensor :: S.ServerT APISensor AppM
serverSensor = readingPostServer

type FullAPI = "auth" :> AuthAPI
               :<|> UnAuthAPI

unAuthAPI :: S.Proxy UnAuthAPI
unAuthAPI = S.Proxy

fullApi :: S.Proxy FullAPI
fullApi = S.Proxy

fullApp :: Config -> Wai.Application
fullApp cfg = S.serve unAuthAPI $ S.enter (readerTToExcept cfg) vendorServer
  :<|> S.enter (readerTToExcept cfg) serverSensor
  :<|> S.enter (readerTToExcept cfg) (loginServer $ secret "johnjacobjingleheimerschmidt")
  :<|> S.serveDirectory "public"

-- fullApp cfg = S.serveWithContext fullApi (authContext cfg) $
              -- (\v -> S.enter (readerTToExcept cfg) (serverGet $ vendorId v)
                -- :<|> S.serveDirectory "auth")
              -- :<|> (S.enter (readerTToExcept cfg) vendorServer
                    -- :<|> S.enter (readerTToExcept cfg) serverPost
                    -- :<|> S.serveDirectory "public")
