{-# LANGUAGE Arrows #-}

module Queries.Vendor where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Vendor

vendorsQuery :: Query VendorColRead
vendorsQuery = queryTable vendorTable

vendorByIdQuery :: VendorID -> Query VendorColRead
vendorByIdQuery id' = proc () -> do
  vendor <- vendorsQuery -< ()
  restrict -< vendorId vendor .== pgInt8 id'
  returnA -< vendor

vendorByUsernameQuery :: Username -> Query VendorColRead
vendorByUsernameQuery uname = proc () -> do
  vendor <- vendorsQuery -< ()
  restrict -< vendorUsername vendor .== pgString uname
  returnA -< vendor
