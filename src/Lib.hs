{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( startApp
    ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import Servant ((:<|>)( .. ), (:>), (:~>), Context( .. ))
import qualified Servant as S
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler, AuthServerData)
import Servant.API.Experimental.Auth (AuthProtect)
-- import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.DateTime (getCurrentTime)
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Concurrent.STM as STM
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import qualified System.Log.FastLogger as FL
import Data.Default.Class (def)
import Data.Maybe (listToMaybe, fromMaybe)
-- import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (decodeUtf8)
import System.Directory (doesFileExist)
import qualified Data.Yaml as Yaml
-- import Web.JWT (binarySecret)

import App (Config ( .. )
           , AppM
           , Environment ( .. )
           , LogTo ( .. )
           , VendorID
           , EnvConfig ( .. )
           , mkConfig
           , checkVendor
           )
import Api.Hub
import Api.Tank
import Api.Reading
import Api.Vendor
import Api.Login
-- import Models.Vendor
import Models.Login
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
      Production -> MidRL.mkRequestLogger $
        def { MidRL.destination = MidRL.Logger logger
            , MidRL.outputFormat = MidRL.Apache MidRL.FromSocket
            }
      Development -> MidRL.mkRequestLogger $ def { MidRL.destination = MidRL.Logger logger }

startApp :: [String] -> IO ()
startApp args = do
  yamlConfig <- Yaml.decodeFileEither "config.yaml"
  envCfg <- case yamlConfig of
    Left err -> fail (Yaml.prettyPrintParseException err)
    Right cfg -> return cfg
  let port = fromMaybe 8080 (envPort envCfg)
      env = fromMaybe Production (envEnvironment envCfg)
  doesFileExist (envCert envCfg) >>= flip unless (fail "Cert file doesn't exist")
  mapM_ (doesFileExist >=> flip unless (fail "Chain file doesn't exist")) (envChain envCfg)
  doesFileExist (envKey envCfg) >>= flip unless (fail "Key file doesn't exist")
  logTo <- case listToMaybe args of
    Just filename -> return $ File filename
    Nothing -> return $ fromMaybe STDOut (envLogTo envCfg)
  pool <- Pool.createPool (openConnection envCfg) PGS.close 1 10 5
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
  cfg <- mkConfig pool logger (envSecret envCfg)
  let tls = Warp.tlsSettingsChain (envCert envCfg) (envChain envCfg) (envKey envCfg)
      settings = Warp.setPort port Warp.defaultSettings
  Warp.runTLS tls settings $ loggerMidware $ appWithSockets cfg $ fullApp envCfg cfg

readerTToExcept :: Config -> AppM :~> ExceptT S.ServantErr IO
readerTToExcept pool = S.Nat (`runReaderT` pool)

type APIWeb = "hubs" :> (HubAPIGet :<|> HubAPIPost)
              :<|> "tanks" :> (TankAPIGet :<|> TankAPIPost)
              :<|> "readings" :> ReadingAPIGet

type APISensor = "readings" :> ReadingAPIPost

type AuthAPI = APIWeb

type UnAuthAPI = "vendors" :> VendorAPI
                 :<|> "post" :> APISensor
                 :<|> "log" :> LoginAPI
                 :<|> S.Raw

serverWeb :: VendorID -> S.ServerT APIWeb AppM
serverWeb v = (hubGetServer v :<|> hubPostServer)
              :<|> (tankGetServer v :<|> tankPostServer)
              :<|> readingGetServer v

serverSensor :: S.ServerT APISensor AppM
serverSensor = readingPostServer

type FullAPI = "auth" :> AuthProtect "jwt-auth" :> AuthAPI
               :<|> UnAuthAPI

fullApi :: S.Proxy FullAPI
fullApi = S.Proxy

type VendorAuth = AuthHandler Wai.Request VendorID

authHandler :: Config -> VendorAuth
authHandler cfg =
  let handler req = case lookup "JWT-Token" (Wai.requestHeaders req) of
        Nothing -> S.throwError (S.err401 { S.errBody = "Missing auth header" })
        Just cookie -> do
          -- liftIO $ FL.pushLogStrLn (getLogger cfg) $ FL.toLogStr (show cookie)
          let token = Token $ decodeUtf8 cookie
              secret = getSecret cfg
              mvid = getVendor secret token
              mtime = getTime secret token
          case (mvid, mtime) of
            (Nothing, _) -> S.throwError (S.err401 { S.errBody = "No user in token" })
            (_, Nothing) -> S.throwError (S.err401 { S.errBody = "No user in token" })
            (Just vid, Just expires) -> do
              now <- liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime
              unless (expires > now) $ S.throwError (S.err401 { S.errBody = "Expired token" })
              let session = getSession cfg
              check <- liftIO $ STM.atomically $ checkVendor session vid (tokenText token)
              liftIO $ FL.pushLogStrLn (getLogger cfg) $ FL.toLogStr (show check)
              if check then return vid
                else S.throwError (S.err401 { S.errBody = "No record of token" })
  in mkAuthHandler handler

type instance AuthServerData (AuthProtect "jwt-auth") = VendorID

authContext :: Config -> Context (VendorAuth ': '[])
authContext cfg = authHandler cfg :. EmptyContext

fullApp :: EnvConfig -> Config -> Wai.Application
fullApp _ cfg =
  S.serveWithContext fullApi (authContext cfg) $
  (S.enter (readerTToExcept cfg) . serverWeb)
  :<|> (S.enter (readerTToExcept cfg) vendorServer
        :<|> S.enter (readerTToExcept cfg) serverSensor
        :<|> S.enter (readerTToExcept cfg) loginServer
        :<|> S.serveDirectory "public")
