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

hubServer :: ServerT HubAPI AppM
hubServer = getHubs
            :<|> getHubById
            :<|> postHub

getHubs :: AppM [HubRead]
getHubs = do
  con <- getConn
  liftIO $ O.runQuery con hubsQuery

getHubById :: HubID -> AppM (Maybe HubRead)
getHubById hubID = do
  con <- getConn
  liftIO $ listToMaybe <$> O.runQuery con (hubByIdQuery hubID)

postHub :: HubWrite -> AppM (Maybe HubID)
postHub hub = do
  con <- getConn
  liftIO $ listToMaybe <$>
    O.runInsertManyReturning con hubTable [hubToPG hub] hubId
