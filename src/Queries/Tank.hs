{-# LANGUAGE Arrows #-}

module Queries.Tank where

import qualified Opaleye as O
import Opaleye ((.==))
import Control.Arrow (returnA)

import App
import Models.Tank
import Models.Hub
import Queries.Hub

tanksQuery :: VendorID -> O.Query TankColRead
tanksQuery v = proc () -> do
  tank <- O.queryTable tankTable -< ()
  hub <- hubsQuery v -< ()
  O.restrict -< tankHub tank .== hubId hub
  returnA -< tank

tankByIdQuery :: VendorID -> TankID -> O.Query TankColRead
tankByIdQuery v tankID = proc () -> do
  tank <- tanksQuery v -< ()
  O.restrict -< tankId tank .== O.pgInt8 tankID
  returnA -< tank

tanksByHubQuery :: VendorID -> HubID -> O.Query TankColRead
tanksByHubQuery v hub = proc () -> do
  tank <- tanksQuery v -< ()
  O.restrict -< tankHub tank .== O.pgInt8 hub
  returnA -< tank

-- yellowOfTank :: O.QueryArr (O.Column O.PGInt8) (O.Column O.PGInt4)
-- yellowOfTank = proc (tankID) -> do
--   tank <- tanksQuery -< ()
--   O.restrict -< tankId tank .== tankID
--   returnA -< tankYellow tank

-- redOfTank :: O.QueryArr (O.Column O.PGInt8) (O.Column O.PGInt4)
-- redOfTank = proc (tankID) -> do
--   tank <- tanksQuery -< ()
--   O.restrict -< tankId tank .== tankID
--   returnA -< tankRed tank
