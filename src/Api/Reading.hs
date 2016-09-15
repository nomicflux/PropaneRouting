{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Reading where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)

import App
import Models.Reading
import Queries.Reading

type ReadingAPI = Get '[JSON] [ReadingRead]
              :<|> Capture "id" ReadingID :> Get '[JSON] (Maybe ReadingRead)
              :<|> "tank" :> Capture "tank" TankID :> Get '[JSON] [ReadingRead]
              :<|> "hub" :> Capture "hub" HubID :> Get '[JSON] [ReadingRead]
              :<|> "yellow" :> Capture "yellow" HubID :> Get '[JSON] [ReadingRead]
              :<|> "red" :> Capture "red" HubID :> Get '[JSON] [ReadingRead]
              :<|> ReqBody '[JSON] ReadingWrite :> Post '[JSON] (Maybe ReadingID)

readingAPI :: Proxy ReadingAPI
readingAPI = Proxy

readingServer :: ServerT ReadingAPI AppM
readingServer = getReadings
            :<|> getReadingById
            :<|> getReadingsByTank
            :<|> getReadingsByHub
            :<|> getYellowReadingsByHub
            :<|> getRedReadingsByHub
            :<|> postReading

getReadings :: AppM [ReadingRead]
getReadings = do
  con <- getConn
  liftIO $ O.runQuery con readingsQuery

getReadingById :: ReadingID -> AppM (Maybe ReadingRead)
getReadingById readingID = do
  con <- getConn
  liftIO $ listToMaybe <$> O.runQuery con (readingByIdQuery readingID)

getReadingsByTank :: TankID -> AppM [ReadingRead]
getReadingsByTank tankID = do
  con <- getConn
  liftIO $ O.runQuery con (O.orderBy (O.asc readingSensorSent) $ readingsByTankQuery tankID)

getReadingsByHub :: HubID -> AppM [ReadingRead]
getReadingsByHub hubID = do
  con <- getConn
  liftIO $ O.runQuery con (readingsByHubQuery hubID)

getYellowReadingsByHub :: HubID -> AppM [ReadingRead]
getYellowReadingsByHub hubID = do
  con <- getConn
  liftIO $ O.runQuery con (yellowReadingsQuery hubID)

getRedReadingsByHub :: HubID -> AppM [ReadingRead]
getRedReadingsByHub hubID = do
  con <- getConn
  liftIO $ O.runQuery con (redReadingsQuery hubID)

postReading :: ReadingWrite -> AppM (Maybe ReadingID)
postReading reading = do
  con <- getConn
  liftIO $ listToMaybe <$>
    O.runInsertManyReturning con readingTable [readingToPG reading] readingId
