{-# LANGUAGE OverloadedStrings #-}

module RunServer
    ( startApp
    ) where

import qualified Network.Wai as Wai
-- import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as MidRL
-- import qualified Servant as S
-- import Control.Monad ((>=>))
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import qualified System.Log.FastLogger as FL
import Data.Default.Class (def)
import Data.Maybe (listToMaybe, fromMaybe)
-- import System.Directory (doesFileExist)
import qualified Data.Yaml as Yaml

import App ( Environment ( .. )
           , LogTo ( .. )
           , EnvConfig ( .. )
           , SecretConfig ( .. )
           , mkConfig
           )
import Notifications.Sockets (appWithSockets)
import Server (fullApp)

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

openConnection :: EnvConfig -> SecretConfig -> IO PGS.Connection
openConnection cfg sec = do
  let connInfo = ConnectionInfo { connUser = envPGUser cfg
                                , connPassword = envPGPassword sec
                                , connDatabase = envPGDatabase cfg
                                }
  con <- PGS.connect (connInfoToPG connInfo)
  -- TODO: Change to purely STM model for DB notifications
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
  yamlConfig <- Yaml.decodeFileEither "config/config.yaml"
  yamlConfigSecret <- Yaml.decodeFileEither "config/config-secret.yaml"
  (envCfg, secret) <- case (yamlConfig, yamlConfigSecret) of
    (Left err, _) -> fail (Yaml.prettyPrintParseException err)
    (_, Left err) -> fail (Yaml.prettyPrintParseException err)
    (Right cfg, Right sec) -> return (cfg, sec)
  let port = fromMaybe 8080 (envPort envCfg)
      env = fromMaybe Production (envEnvironment envCfg)
  -- doesFileExist (envCert envCfg) >>= flip unless (fail "Cert file doesn't exist")
  -- mapM_ (doesFileExist >=> flip unless (fail "Chain file doesn't exist")) (envChain envCfg)
  -- doesFileExist (envKey envCfg) >>= flip unless (fail "Key file doesn't exist")
  logTo <- case listToMaybe args of
    Just filename -> return $ File filename
    Nothing -> return $ fromMaybe STDOut (envLogTo envCfg)
  pool <- Pool.createPool (openConnection envCfg secret) PGS.close 1 10 5
  logger <- makeLogger logTo
  loggerMidware <- makeMiddleware logger env
  FL.pushLogStrLn logger $ FL.toLogStr $ mconcat
    [ "Listening on port ", show port
    , " at level ", show env
    , " and logging to ", show logTo
    , if null args then " with no args " else " with args ", unwords args
    ]
  cfg <- mkConfig pool logger (envSecret secret)
  -- let tls = Warp.tlsSettingsChain (envCert envCfg) (envChain envCfg) (envKey envCfg)
      -- settings = Warp.setPort port Warp.defaultSettings
  -- Warp.runTLS tls settings $ loggerMidware $ appWithSockets cfg $ fullApp envCfg cfg
  Warp.run port $ loggerMidware $ appWithSockets cfg $ fullApp envCfg cfg
