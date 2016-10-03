{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Reading where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import qualified Data.DateTime as DT

import App
import Models.Reading
import Queries.Reading

type ReadingAPI =
  Get '[JSON] [ReadingRead]
  :<|> Capture "id" ReadingID :> Get '[JSON] (Maybe ReadingRead)
  :<|> "tank" :> Capture "tank" TankID :> QueryParam "seconds" Integer :> QueryParam "lastreading" ReadingID :> Get '[JSON] [ReadingRead]
  :<|> "hub" :> Capture "hub" HubID :> Get '[JSON] [ReadingRead]
  -- :<|> "yellow" :> Capture "yellow" HubID :> Get '[JSON] [ReadingRead]
  -- :<|> "red" :> Capture "red" HubID :> Get '[JSON] [ReadingRead]
  :<|> ReqBody '[JSON] ReadingWrite :> Post '[JSON] (Maybe ReadingID)

readingAPI :: Proxy ReadingAPI
readingAPI = Proxy

readingServer :: VendorID -> ServerT ReadingAPI AppM
readingServer v = getReadings v
            :<|> getReadingById v
            :<|> getReadingsByTank v
            :<|> getReadingsByHub v
            -- :<|> getYellowReadingsByHub
            -- :<|> getRedReadingsByHub
            :<|> postReading v

getReadings :: VendorID -> AppM [ReadingRead]
getReadings v = do
  con <- getConn
  liftIO $ O.runQuery con (readingsQuery v)

getReadingById :: VendorID -> ReadingID -> AppM (Maybe ReadingRead)
getReadingById v readingID = do
  con <- getConn
  liftIO $ listToMaybe <$> O.runQuery con (readingByIdQuery v readingID)

getReadingsByTank :: VendorID -> TankID -> Maybe Integer -> Maybe ReadingID -> AppM [ReadingRead]
getReadingsByTank v tankID mseconds mreading = do
  con <- getConn
  now <- liftIO DT.getCurrentTime
  addToLogger $ show mseconds
  addToLogger $ show mreading
  let query = case (mseconds, mreading) of
        (Nothing, Nothing) -> readingsByTankQuery v tankID
        (Nothing, Just rid) -> readingsByTankIdQuery v tankID rid
        (Just seconds, Nothing) ->
          readingsByTankLimitQuery v tankID $ DT.addSeconds (-1*seconds) now
        (Just seconds, Just rid) ->
          readingsByTankLimitIdQuery v tankID (DT.addSeconds (-1*seconds) now) rid
  liftIO $ O.runQuery con (O.orderBy (O.asc readingSensorSent) query)

getReadingsByHub :: VendorID -> HubID -> AppM [ReadingRead]
getReadingsByHub v hubID = do
  con <- getConn
  liftIO $ O.runQuery con (readingsByHubQuery v hubID)

-- getYellowReadingsByHub :: VendorID -> HubID -> AppM [ReadingRead]
-- getYellowReadingsByHub v hubID = do
--   con <- getConn
--   liftIO $ O.runQuery con (yellowReadingsQuery hubID)

-- getRedReadingsByHub :: HubID -> AppM [ReadingRead]
-- getRedReadingsByHub hubID = do
--   con <- getConn
--   liftIO $ O.runQuery con (redReadingsQuery hubID)

postReading :: VendorID -> ReadingWrite -> AppM (Maybe ReadingID)
postReading _ reading = do
  con <- getConn
  liftIO $ listToMaybe <$>
    O.runInsertManyReturning con readingTable [readingToPG reading] readingId
