{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Reading where

import qualified Opaleye as O
import Control.Monad (mzero)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import GHC.Generics (Generic)

import App

data Reading' id tank value dbdt sdt = Reading
                                       { readingId         :: id
                                       , readingTank       :: tank
                                       , readingValue      :: value
                                       , readingDbReceived :: dbdt
                                       , readingSensorSent :: sdt
                                       } deriving (Generic)

type ReadingRead = Reading' ReadingID TankID Reading DateTime DateTime
type ReadingWrite = Reading' (Maybe ReadingID) TankID Reading (Maybe DateTime) DateTime
type ReadingColRead = Reading' (O.Column O.PGInt8)
                               (O.Column O.PGInt8)
                               (O.Column O.PGInt4)
                               (O.Column O.PGTimestamptz)
                               (O.Column O.PGTimestamptz)
type ReadingColWrite = Reading' (Maybe (O.Column O.PGInt8))
                                (O.Column O.PGInt8)
                                (O.Column O.PGInt4)
                                (Maybe (O.Column O.PGTimestamptz))
                               (O.Column O.PGTimestamptz)

instance ToJSON ReadingRead where
  toJSON reading = object [ "id"        .= readingId reading
                          , "tank"      .= readingTank reading
                          , "value"     .= readingValue reading
                          , "dbreceived" .= readingDbReceived reading
                          , "sensorsent" .= readingSensorSent reading
                          ]

instance FromJSON ReadingWrite where
  parseJSON (Object o) = Reading <$>
                         o .:? "id" <*>
                         o .: "tank" <*>
                         o .: "value" <*>
                         o .:? "dbreceived" <*>
                         o .: "sensorsent"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pReading" ''Reading')

readingTable :: O.Table ReadingColWrite ReadingColRead
readingTable = O.Table "readings" (pReading Reading { readingId = O.optional "id"
                                                    , readingTank = O.required "tank"
                                                    , readingValue = O.required "value"
                                                    , readingDbReceived = O.optional "dbreceived"
                                                    , readingSensorSent = O.required "sensorsent"
                                                    })

readingToPG :: ReadingWrite -> ReadingColWrite
readingToPG = pReading Reading { readingId = const Nothing
                               , readingTank = O.pgInt8
                               , readingValue = O.pgInt4
                               , readingDbReceived = const Nothing
                               , readingSensorSent = O.pgUTCTime
                               }
