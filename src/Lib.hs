{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import Servant ((:<|>)( .. ), (:>), (:~>))
import qualified Servant as S
-- import qualified Servant.API.BasicAuth as S
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import qualified System.Log.FastLogger as FL
import Data.Default.Class (def, Default)
import Data.Maybe (listToMaybe)

import App (Config ( .. )
           , AppM
           , Environment ( .. )
           , LogTo ( .. )
           , lookupEnvDefault
           , mkConfig
           )
import Api.Hub
import Api.Tank
import Api.Reading
import Api.Vendor
import Notifications.Sockets (appWithSockets)

data ConnectionInfo = ConnectionInfo
                      { connUser :: String
                      , connPassword :: String
                      , connDatabase :: String
                      }

instance Default ConnectionInfo where
  def = ConnectionInfo
        { connUser = "gasmasters"
        , connPassword = "oscarMeyerPadThai"
        , connDatabase = "gasmasters"
        }

getConnInfo :: IO ConnectionInfo
getConnInfo =
  ConnectionInfo <$>
    lookupEnvDefault "GM_PG_USER" (connUser def) <*>
    lookupEnvDefault "GM_PG_PWD" (connPassword def) <*>
    lookupEnvDefault "GM_PG_DB" (connDatabase def)

connInfoToPG :: ConnectionInfo -> PGS.ConnectInfo
connInfoToPG connInfo = PGS.defaultConnectInfo
                        { PGS.connectUser = connUser connInfo
                        , PGS.connectPassword = connPassword connInfo
                        , PGS.connectDatabase = connDatabase connInfo
                        }

openConnection :: IO PGS.Connection
openConnection = do
  connInfo <- getConnInfo
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
  port <- lookupEnvDefault "GM_PORT" 8080
  env <- lookupEnvDefault "GM_ENV" Production
  logTo <- case listToMaybe args of
    Just filename -> return $ File filename
    Nothing -> lookupEnvDefault "GM_LOG" STDOut
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- makeLogger logTo
  loggerMidware <- makeMiddleware logger env
  FL.pushLogStrLn logger $ FL.toLogStr $
    "Listening on port " ++ show port ++ " at level " ++ show env ++ " and logging to "  ++ show logTo ++ (if null args then " with no args " else " with args " ++ unwords args)
  -- specsToDir [hubSpec, tankSpec, readingSpec] "src/Elm"
  cfg <- mkConfig pool logger
  Warp.run port $ loggerMidware $ appWithSockets cfg $ fullApp cfg

readerTToExcept :: Config -> AppM :~> ExceptT S.ServantErr IO
readerTToExcept pool = S.Nat (\r -> runReaderT r pool)

type API' = "hubs" :> HubAPI
      :<|> "tanks" :> TankAPI
      :<|> "readings" :> ReadingAPI
      :<|> "vendors" :> VendorAPI

type AuthAPI = API' :<|> S.Raw

server' :: S.ServerT API' AppM
server' = hubServer
    :<|> tankServer
    :<|> readingServer
    :<|> vendorServer

type FullAPI = "auth" :> AuthAPI
               :<|> S.Raw

fullApi :: S.Proxy FullAPI
fullApi = S.Proxy

fullApp :: Config -> Wai.Application
fullApp cfg = S.serve fullApi $
              (S.enter (readerTToExcept cfg) server'
               :<|> S.serveDirectory "auth")
              :<|> S.serveDirectory "public"
