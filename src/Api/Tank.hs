{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Tank where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)

import App
import Models.Tank
import Queries.Tank

type TankAPI = Get '[JSON] [TankRead]
              :<|> Capture "id" TankID :> Get '[JSON] (Maybe TankRead)
              :<|> "hub" :> Capture "hub" HubID :> Get '[JSON] [TankRead]
              :<|> ReqBody '[JSON] TankWrite :> Post '[JSON] (Maybe TankID)

tankAPI :: Proxy TankAPI
tankAPI = Proxy

tankServer :: ServerT TankAPI AppM
tankServer = getTanks
            :<|> getTankById
            :<|> getTanksByHub
            :<|> postTank

getTanks :: AppM [TankRead]
getTanks = do
  con <- getConn
  liftIO $ O.runQuery con tanksQuery

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
