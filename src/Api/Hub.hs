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

type HubAPIGet = Get '[JSON] [HubRead]
              :<|> Capture "id" HubID :> Get '[JSON] (Maybe HubRead)

type HubAPIPost = ReqBody '[JSON] HubWrite :> Post '[JSON] (Maybe HubID)

hubAPIGet :: Proxy HubAPIGet
hubAPIGet = Proxy

hubAPIPost :: Proxy HubAPIPost
hubAPIPost = Proxy

hubGetServer :: VendorID -> ServerT HubAPIGet AppM
hubGetServer v = getHubs v
                 :<|> getHubById v

hubPostServer :: ServerT HubAPIPost AppM
hubPostServer = postHub

getHubs :: VendorID -> AppM [HubRead]
getHubs v = do
  con <- getConn
  liftIO $ O.runQuery con (hubsQuery v)

getHubById :: VendorID -> HubID -> AppM (Maybe HubRead)
getHubById v hubID = do
  con <- getConn
  liftIO $ listToMaybe <$> O.runQuery con (hubByIdQuery v hubID)

postHub :: HubWrite -> AppM (Maybe HubID)
postHub hub = do
  con <- getConn
  liftIO $ listToMaybe <$>
    O.runInsertManyReturning con hubTable [hubToPG hub] hubId
