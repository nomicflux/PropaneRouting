{-# LANGUAGE Arrows #-}

module Queries.Hub where

import qualified Opaleye as O
import Opaleye ((.==))
import Control.Arrow (returnA)

import App
import Models.Hub

hubsQuery :: O.Query HubColRead
hubsQuery = O.queryTable hubTable

hubByIdQuery :: HubID -> O.Query HubColRead
hubByIdQuery hubID = proc () -> do
  hub <- hubsQuery -< ()
  O.restrict -< hubId hub .== O.pgInt8 hubID
  returnA -< hub
