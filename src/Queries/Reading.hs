{-# LANGUAGE Arrows #-}

module Queries.Reading where

import qualified Opaleye as O
import Opaleye ((.==), (.>=), (.>))
import Control.Arrow (returnA)
import Data.DateTime (DateTime)

import App
import Models.Reading
import Models.Tank
import Queries.Tank

readingsQuery :: VendorID -> O.Query ReadingColRead
readingsQuery v = proc () -> do
  reading <- O.queryTable readingTable -< ()
  tank <- tanksQuery v -< ()
  O.restrict -< readingTank reading .== tankId tank
  returnA -< reading

readingByIdQuery :: VendorID -> ReadingID -> O.Query ReadingColRead
readingByIdQuery v readingID = proc () -> do
  reading <- readingsQuery v -< ()
  O.restrict -< readingId reading .== O.pgInt8 readingID
  returnA -< reading

readingsByTankQuery :: VendorID -> TankID -> O.Query ReadingColRead
readingsByTankQuery v tank = proc () -> do
  reading <- readingsQuery v -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  returnA -< reading

-- dateLimit :: O.QueryArr (O.Column O.PGTimestamptz) ReadingColRead
-- dateLimit = proc (v, since) -> do
--   reading <- readingsQuery v -< ()
--   O.restrict -< readingSensorSent reading .>= since
--   returnA -< reading

-- idLimit :: O.QueryArr (O.Column O.PGInt8) ReadingColRead
-- idLimit = proc (v, rid) -> do
--   reading <- readingsQuery v -< ()
--   O.restrict -< readingId reading .> rid
--   returnA -< reading

readingsByTankLimitQuery :: VendorID -> TankID -> DateTime -> O.Query ReadingColRead
readingsByTankLimitQuery v tank since = proc () -> do
  reading <- readingsQuery v -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  O.restrict -< readingSensorSent reading .>= O.pgUTCTime since
  returnA -< reading

readingsByTankIdQuery :: VendorID -> TankID -> ReadingID -> O.Query ReadingColRead
readingsByTankIdQuery v tank lastReading = proc () -> do
  reading <- readingsQuery v -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  O.restrict -< readingId reading .> O.pgInt8 lastReading
  returnA -< reading

readingsByTankLimitIdQuery :: VendorID -> TankID -> DateTime -> ReadingID -> O.Query ReadingColRead
readingsByTankLimitIdQuery v tank since lastReading = proc () -> do
  reading <- readingsQuery v -< ()
  O.restrict -< readingTank reading .== O.pgInt8 tank
  O.restrict -< readingSensorSent reading .>= O.pgUTCTime since
  O.restrict -< readingId reading .> O.pgInt8 lastReading
  returnA -< reading

readingsByHubQuery :: VendorID -> HubID -> O.Query ReadingColRead
readingsByHubQuery v hub = proc () -> do
  tank <- tanksByHubQuery v hub -< ()
  reading <- readingsQuery v -< ()
  O.restrict -< readingTank reading .== tankId tank
  returnA -< reading

-- yellowReadingsQuery :: HubID -> O.Query ReadingColRead
-- yellowReadingsQuery hub = proc () -> do
--   reading <- readingsByHubQuery hub -< ()
--   yellow <- yellowOfTank -< readingTank reading
--   O.restrict -< readingValue reading .<= yellow
--   returnA -< reading

-- redReadingsQuery :: HubID -> O.Query ReadingColRead
-- redReadingsQuery hub = proc () -> do
--   reading <- readingsByHubQuery hub -< ()
--   red <- redOfTank -< readingTank reading
--   O.restrict -< readingValue reading .<= red
--   returnA -< reading
