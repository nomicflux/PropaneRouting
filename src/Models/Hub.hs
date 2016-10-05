{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Hub where

import qualified Opaleye as O
import Control.Monad (mzero)
import Data.Aeson
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import GHC.Generics (Generic)

import App

data Hub' id name lat lng vendor = Hub
                            { hubId         :: id
                            , hubName       :: name
                            , hubLat        :: lat
                            , hubLng        :: lng
                            , hubVendor     :: vendor
                            } deriving (Generic)

type HubRead = Hub' HubID String Lat Lng VendorID
type HubWrite = Hub' (Maybe HubID) String Lat Lng VendorID
type HubColRead = Hub' (O.Column O.PGInt8)
                       (O.Column O.PGText)
                       (O.Column O.PGFloat8)
                       (O.Column O.PGFloat8)
                       (O.Column O.PGInt8)
type HubColWrite = Hub' (Maybe (O.Column O.PGInt8))
                        (O.Column O.PGText)
                        (O.Column O.PGFloat8)
                        (O.Column O.PGFloat8)
                        (O.Column O.PGInt8)

instance ToJSON HubRead where
  toJSON hub = object [ "id"      .= hubId hub
                      , "name"    .= hubName hub
                      , "lat"     .= hubLat hub
                      , "lng"     .= hubLng hub
                      , "vendor"  .= hubVendor hub
                      ]

instance FromJSON HubWrite where
  parseJSON (Object o) = Hub <$>
                         o .:? "id" <*>
                         o .: "name" <*>
                         o .: "lat" <*>
                         o .: "lng" <*>
                         o .: "vendor"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pHub" ''Hub')

hubTable :: O.Table HubColWrite HubColRead
hubTable = O.Table "hubs" (pHub Hub { hubId = O.optional "id"
                                    , hubName = O.required "name"
                                    , hubLat = O.required "lat"
                                    , hubLng = O.required "lng"
                                    , hubVendor = O.required "vendor_id"
                                    })

hubToPG :: HubWrite -> HubColWrite
hubToPG = pHub Hub { hubId = const Nothing
                   , hubName = O.pgString
                   , hubLat = O.pgDouble
                   , hubLng = O.pgDouble
                   , hubVendor = O.pgInt8
                   }
