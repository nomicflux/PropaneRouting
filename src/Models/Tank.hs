{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Tank where

import qualified Opaleye as O
import Control.Monad (mzero)
import Data.Aeson
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import GHC.Generics (Generic)

import App

data Tank' id hub name yellow red lat lng = Tank
                                            { tankId         :: id
                                            , tankHub        :: hub
                                            , tankName       :: name
                                            , tankYellow     :: yellow
                                            , tankRed        :: red
                                            , tankLat        :: lat
                                            , tankLng        :: lng
                                            } deriving (Generic)

type TankRead = Tank' TankID HubID String Reading Reading Lat Lng
type TankWrite = Tank' (Maybe TankID) HubID String (Maybe Reading) (Maybe Reading) Lat Lng
type TankColRead = Tank' (O.Column O.PGInt8)
                         (O.Column O.PGInt8)
                         (O.Column O.PGText)
                         (O.Column O.PGInt4)
                         (O.Column O.PGInt4)
                         (O.Column O.PGFloat8)
                         (O.Column O.PGFloat8)
type TankColWrite = Tank' (Maybe (O.Column O.PGInt8))
                          (O.Column O.PGInt8)
                          (O.Column O.PGText)
                          (Maybe (O.Column O.PGInt4))
                          (Maybe (O.Column O.PGInt4))
                          (O.Column O.PGFloat8)
                          (O.Column O.PGFloat8)

instance ToJSON TankRead where
  toJSON tank = object [ "id"      .= tankId tank
                       , "hub"     .= tankHub tank
                       , "name"    .= tankName tank
                       , "yellow"  .= tankYellow tank
                       , "red"     .= tankRed tank
                       , "lat"     .= tankLat tank
                       , "lng"     .= tankLng tank
                       ]

instance FromJSON TankWrite where
  parseJSON (Object o) = Tank <$>
                         o .:? "id" <*>
                         o .: "hub" <*>
                         o .: "name" <*>
                         o .:? "yellow" <*>
                         o .:? "red" <*>
                         o .: "lat" <*>
                         o .: "lng"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pTank" ''Tank')

tankTable :: O.Table TankColWrite TankColRead
tankTable = O.Table "tanks" (pTank Tank { tankId = O.optional "id"
                                        , tankHub = O.required "hub"
                                        , tankName = O.required "name"
                                        , tankYellow = O.optional "yellow_threshold"
                                        , tankRed = O.optional "red_threshold"
                                        , tankLat = O.required "lat"
                                        , tankLng = O.required "lng"
                                        })

tankToPG :: TankWrite -> TankColWrite
tankToPG = pTank Tank { tankId = const Nothing
                      , tankHub = O.pgInt8
                      , tankName = O.pgString
                      , tankYellow = fmap O.pgInt4
                      , tankRed = fmap O.pgInt4
                      , tankLat = O.pgDouble
                      , tankLng = O.pgDouble
                      }
