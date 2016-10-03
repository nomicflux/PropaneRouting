{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Hub where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)

import App
import Models.Hub
import Queries.Hub

type HubAPI = Get '[JSON] [HubRead]
              :<|> Capture "id" HubID :> Get '[JSON] (Maybe HubRead)
              :<|> ReqBody '[JSON] HubWrite :> Post '[JSON] (Maybe HubID)

hubAPI :: Proxy HubAPI
hubAPI = Proxy

hubServer :: VendorID -> ServerT HubAPI AppM
hubServer v = getHubs v
              :<|> getHubById v
              :<|> postHub v

getHubs :: VendorID -> AppM [HubRead]
getHubs v = do
  con <- getConn
  liftIO $ O.runQuery con (hubsQuery v)

getHubById :: VendorID -> HubID -> AppM (Maybe HubRead)
getHubById v hubID = do
  con <- getConn
  liftIO $ listToMaybe <$> O.runQuery con (hubByIdQuery v hubID)

postHub :: VendorID -> HubWrite -> AppM (Maybe HubID)
postHub v hub = do
  con <- getConn
  liftIO $ listToMaybe <$>
    O.runInsertManyReturning con hubTable [hubToPG v hub] hubId
