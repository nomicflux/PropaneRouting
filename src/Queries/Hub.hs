{-# LANGUAGE Arrows #-}

module Queries.Hub where

import qualified Opaleye as O
import Opaleye ((.==))
import Control.Arrow (returnA)

import App
import Models.Hub

hubsQuery :: VendorID -> O.Query HubColRead
hubsQuery v = proc () -> do
  hub <- O.queryTable hubTable -< ()
  O.restrict -< hubVendor hub .== O.pgInt8 v
  returnA -< hub

hubByIdQuery :: VendorID -> HubID -> O.Query HubColRead
hubByIdQuery v hubID = proc () -> do
  hub <- hubsQuery v -< ()
  O.restrict -< hubId hub .== O.pgInt8 hubID
  returnA -< hub
