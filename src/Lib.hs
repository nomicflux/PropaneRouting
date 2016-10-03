{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import Servant ((:<|>)( .. ), (:>), (:~>), Context( .. ), BasicAuthCheck)
import qualified Servant as S
import Servant.API.BasicAuth (BasicAuth)
import qualified Servant.API.BasicAuth as SBA
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import qualified System.Log.FastLogger as FL
import Data.Default.Class (def, Default)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Char8 as BS

import App (Config ( .. )
           , AppM
           , Environment ( .. )
           , LogTo ( .. )
           , VendorID
           , lookupEnvDefault
           , mkConfig
           )
import Api.Hub
import Api.Tank
import Api.Reading
import Api.Vendor
import Models.Vendor
import Queries.Vendor
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

type AuthAPI = API' :<|> S.Raw

type UnAuthAPI = "vendors" :> VendorAPI
                 :<|> S.Raw

server' :: VendorID -> S.ServerT API' AppM
server' v = hubServer v
    :<|> tankServer v
    :<|> readingServer v

type FullAPI = "auth" :> BasicAuth "vendor" VendorRead :> AuthAPI
               :<|> UnAuthAPI

authCheck :: Config -> BasicAuthCheck VendorRead
authCheck cfg =
  let
    logger = getLogger cfg
    logMsg msg = FL.pushLogStrLn logger . FL.toLogStr $ msg
    check (SBA.BasicAuthData uname pwd) = do
      con <- Pool.withResource (getPool cfg) return
      dbVendor <- liftIO $ listToMaybe <$> O.runQuery con (vendorByUsernameQuery $ BS.unpack uname)
      case dbVendor of
        Nothing -> logMsg ("NoSuchUser: " ++ show uname) >> return S.NoSuchUser
        Just v -> if authPassword v pwd
          then return (S.Authorized v)
          else logMsg ("BadPassword: " ++ show uname) >> return S.BadPassword
  in
    S.BasicAuthCheck check

authContext :: Config -> Context (BasicAuthCheck VendorRead ': '[])
authContext cfg = authCheck cfg :. EmptyContext

fullApi :: S.Proxy FullAPI
fullApi = S.Proxy

fullApp :: Config -> Wai.Application
fullApp cfg = S.serveWithContext fullApi (authContext cfg) $
              (\v -> S.enter (readerTToExcept cfg) (server' $ vendorId v)
                :<|> S.serveDirectory "auth")
              :<|> (S.enter (readerTToExcept cfg) vendorServer
                    :<|> S.serveDirectory "public")
