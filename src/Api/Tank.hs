{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Tank where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
-- import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Notification as PGSN
-- import qualified Data.ByteString as BS
import Data.ByteString.Char8 (readInteger)

import App
import Models.Tank
import Queries.Tank

type TankAPI = Get '[JSON] [TankRead]
              :<|> Capture "id" TankID :> Get '[JSON] (Maybe TankRead)
              :<|> "notifications" :> Get '[JSON] (Maybe ReadingID)
              :<|> "hub" :> Capture "hub" HubID :> Get '[JSON] [TankRead]
              :<|> ReqBody '[JSON] TankWrite :> Post '[JSON] (Maybe TankID)
              :<|> "register" :> Post '[JSON] Bool
              :<|> "unregister" :> Post '[JSON] Bool

tankAPI :: Proxy TankAPI
tankAPI = Proxy

tankServer :: ServerT TankAPI AppM
tankServer = getTanks
            :<|> getTankById
            :<|> getTankNotifications
            :<|> getTanksByHub
            :<|> postTank
            :<|> registerForNotifications
            :<|> unregisterForNotifications

getTanks :: AppM [TankRead]
getTanks = do
  con <- getConn
  liftIO $ O.runQuery con tanksQuery

registerForNotifications :: AppM Bool
registerForNotifications = loop 3
  where
    loop :: Int -> AppM Bool
    loop 0 = return False
    loop n = do
      res <- listenToNotifications
      if res
        then return True
        else loop (n-1)

unregisterForNotifications :: AppM Bool
unregisterForNotifications = loop 3
  where
    loop :: Int -> AppM Bool
    loop 0 = return False
    loop n = do
      res <- unlistenToNotifications
      if res
        then return True
        else loop (n - 1)

getTankNotifications :: AppM (Maybe ReadingID)
getTankNotifications = do
  con <- getConn
  mnotification <- liftIO $ PGSN.getNotificationNonBlocking con
  return $ do
    notification <- mnotification
    idplus <- readInteger $ PGSN.notificationData notification
    return $ fromIntegral . fst $ idplus

getTankById :: TankID -> AppM (Maybe TankRead)
getTankById tankID = do
  con <- getConn
  liftIO $ listToMaybe <$> O.runQuery con (tankByIdQuery tankID)

getTanksByHub :: HubID -> AppM [TankRead]
getTanksByHub hubID = do
  con <- getConn
  liftIO $ O.runQuery con (tanksByHubQuery hubID)

postTank :: TankWrite -> AppM (Maybe TankID)
postTank tank = do
  con <- getConn
  liftIO $ listToMaybe <$>
    O.runInsertManyReturning con tankTable [tankToPG tank] tankId
