{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Tank where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
-- import qualified Database.PostgreSQL.Simple as PGS
-- import qualified Database.PostgreSQL.Simple.Notification as PGSN
-- import qualified Data.ByteString as BS
-- import Data.ByteString.Char8 (readInteger)

import App
import Models.Tank
import Queries.Tank

type TankAPIGet = Get '[JSON] [TankRead]
              :<|> Capture "id" TankID :> Get '[JSON] (Maybe TankRead)
              :<|> "hub" :> Capture "hub" HubID :> Get '[JSON] [TankRead]

type TankAPIPost = ReqBody '[JSON] TankWrite :> Post '[JSON] (Maybe TankID)

tankAPIGet :: Proxy TankAPIGet
tankAPIGet = Proxy

tankGetServer :: VendorID -> ServerT TankAPIGet AppM
tankGetServer v = getTanks v
               :<|> getTankById v
               :<|> getTanksByHub v
tankPostServer :: ServerT TankAPIPost AppM
tankPostServer = postTank

getTanks :: VendorID -> AppM [TankRead]
getTanks v = do
  con <- getConn
  liftIO $ O.runQuery con (tanksQuery v)

getTankById :: VendorID -> TankID -> AppM (Maybe TankRead)
getTankById v tankID = do
  con <- getConn
  liftIO $ listToMaybe <$> O.runQuery con (tankByIdQuery v tankID)

getTanksByHub :: VendorID -> HubID -> AppM [TankRead]
getTanksByHub v hubID = do
  con <- getConn
  liftIO $ O.runQuery con (tanksByHubQuery v hubID)

postTank :: TankWrite -> AppM (Maybe TankID)
postTank tank = do
  con <- getConn
  liftIO $ listToMaybe <$>
    O.runInsertManyReturning con tankTable [tankToPG tank] tankId
