{-# LANGUAGE OverloadedStrings #-}

module Notifications.Sockets where

import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai as Wai
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (race_)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Notification as PGSN
import qualified Data.ByteString.Lazy as BS
import Data.Pool (withResource)
import System.Log.FastLogger (pushLogStrLn)

import App

type SocketApp = ReaderT Config IO

socketApp :: Config -> WS.PendingConnection -> IO ()
socketApp cfg pending = do
  wscon <- WS.acceptRequest pending
  dbcon <- withResource (getPool cfg) return
  _ <- PGS.execute_ dbcon "LISTEN addedreading"
  pushLogStrLn (getLogger cfg) "Adding socket"
  WS.forkPingThread wscon 15
  runSocketApp cfg (sendLoop wscon) (receiveLoop wscon) `finally` (pushLogStrLn (getLogger cfg) "Closing socket" >> PGS.execute_ dbcon "UNLISTEN addedreading")

appWithSockets :: Config -> Wai.Application -> Wai.Application
appWithSockets cfg = WaiWS.websocketsOr WS.defaultConnectionOptions (socketApp cfg)

runSocketApp :: Config -> SocketApp a -> SocketApp b -> IO ()
runSocketApp cfg left right = (runReaderT left cfg) `race_` (runReaderT right cfg)

sendLoop :: WS.Connection -> SocketApp ()
sendLoop wscon = forever $ do
  dbcon <- getConn
  mnotification <- liftIO $ PGSN.getNotificationNonBlocking dbcon
  case mnotification of
    Nothing -> return ()
    Just notification -> do
      let note = BS.fromStrict . PGSN.notificationData $ notification
      addToLogger (show note)
      liftIO $
        WS.sendDataMessage wscon (WS.Text . BS.fromStrict . PGSN.notificationData $ notification)

receiveLoop :: WS.Connection -> SocketApp ()
receiveLoop wscon = forever $ do
  rcv <- liftIO $ WS.receive wscon
  addToLogger (show rcv)
