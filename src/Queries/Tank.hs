{-# LANGUAGE Arrows #-}

module Queries.Tank where

import qualified Opaleye as O
import Opaleye ((.==))
import Control.Arrow (returnA)

import App
import Models.Tank

tanksQuery :: O.Query TankColRead
tanksQuery = O.queryTable tankTable

tankByIdQuery :: TankID -> O.Query TankColRead
tankByIdQuery tankID = proc () -> do
  tank <- tanksQuery -< ()
  O.restrict -< tankId tank .== O.pgInt8 tankID
  returnA -< tank

tanksByHubQuery :: HubID -> O.Query TankColRead
tanksByHubQuery hub = proc () -> do
  tank <- tanksQuery -< ()
  O.restrict -< tankHub tank .== O.pgInt8 hub
  returnA -< tank

yellowOfTank :: O.QueryArr (O.Column O.PGInt8) (O.Column O.PGInt4)
yellowOfTank = proc (tankID) -> do
  tank <- tanksQuery -< ()
  O.restrict -< tankId tank .== tankID
  returnA -< tankYellow tank

redOfTank :: O.QueryArr (O.Column O.PGInt8) (O.Column O.PGInt4)
redOfTank = proc (tankID) -> do
  tank <- tanksQuery -< ()
  O.restrict -< tankId tank .== tankID
  returnA -< tankRed tank
