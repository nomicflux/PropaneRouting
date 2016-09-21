{-# LANGUAGE Arrows #-}

module Queries.Reading where

import qualified Opaleye as O
import Opaleye ((.==), (.<=), (.>=), (.>))
import Control.Arrow (returnA)
import Data.DateTime (DateTime)

import App
import Models.Reading
import Models.Tank
import Queries.Tank

readingsQuery :: O.Query ReadingColRead
readingsQuery = O.queryTable readingTable

readingByIdQuery :: ReadingID -> O.Query ReadingColRead
readingByIdQuery readingID = proc () -> do
  reading <- readingsQuery -< ()
  O.restrict -< readingId reading .== O.pgInt8 readingID
  returnA -< reading

readingsByTankQuery :: TankID -> O.Query ReadingColRead
readingsByTankQuery tank = proc () -> do
  reading <- readingsQuery -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  returnA -< reading

readingsByTankLimitQuery :: TankID -> DateTime -> O.Query ReadingColRead
readingsByTankLimitQuery tank since = proc () -> do
  reading <- readingsQuery -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  O.restrict -< readingSensorSent reading .>= O.pgUTCTime since
  returnA -< reading

readingsByTankIdQuery :: TankID -> ReadingID -> O.Query ReadingColRead
readingsByTankIdQuery tank lastReading = proc () -> do
  reading <- readingsQuery -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  O.restrict -< readingId reading .> O.pgInt8 lastReading
  returnA -< reading

readingsByTankLimitIdQuery :: TankID -> DateTime -> ReadingID -> O.Query ReadingColRead
readingsByTankLimitIdQuery tank since lastReading = proc () -> do
  reading <- readingsQuery -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  O.restrict -< readingSensorSent reading .>= O.pgUTCTime since
  O.restrict -< readingId reading .> O.pgInt8 lastReading
  returnA -< reading

readingsByHubQuery :: HubID -> O.Query ReadingColRead
readingsByHubQuery hub = proc () -> do
  tank <- tanksByHubQuery hub -< ()
  reading <- readingsQuery -< ()
  O.restrict -< readingTank reading .== tankId tank
  returnA -< reading

yellowReadingsQuery :: HubID -> O.Query ReadingColRead
yellowReadingsQuery hub = proc () -> do
  reading <- readingsByHubQuery hub -< ()
  yellow <- yellowOfTank -< readingTank reading
  O.restrict -< readingValue reading .<= yellow
  returnA -< reading

redReadingsQuery :: HubID -> O.Query ReadingColRead
redReadingsQuery hub = proc () -> do
  reading <- readingsByHubQuery hub -< ()
  red <- redOfTank -< readingTank reading
  O.restrict -< readingValue reading .<= red
  returnA -< reading
