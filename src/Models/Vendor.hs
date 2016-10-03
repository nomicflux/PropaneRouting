{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Vendor where

import qualified Opaleye as O
import Control.Monad (mzero)
import Crypto.PasswordStore
import Data.Aeson
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import GHC.Generics (Generic)

import App

data Vendor' id name street city state zipCode username pwd = Vendor
                            { vendorId         :: id
                            , vendorName       :: name
                            , vendorStreet     :: street
                            , vendorCity       :: city
                            , vendorState      :: state
                            , vendorZip        :: zipCode
                            , vendorUsername   :: username
                            , vendorPassword   :: pwd
                            } deriving (Generic)

type VendorRead = Vendor' VendorID String String String String String Username Password
type VendorWrite = Vendor' (Maybe VendorID) String String String String String Username String
type VendorColRead = Vendor' (O.Column O.PGInt8)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGBytea)
type VendorColWrite = Vendor' (Maybe (O.Column O.PGInt8))
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGText)
                     (O.Column O.PGBytea)

$(makeAdaptorAndInstance "pVendor" ''Vendor')

instance ToJSON VendorRead where
  toJSON v = object [ "id"       .= vendorId v
                    , "name"     .= vendorName v
                    , "street"   .= vendorStreet v
                    , "city"     .= vendorCity v
                    , "state"    .= vendorState v
                    , "zipcode"  .= vendorZip v
                    , "username" .= vendorUsername v
                    ]

instance FromJSON VendorWrite where
  parseJSON (Object o) = Vendor <$>
                         o .:? "id" <*>
                         o .: "name" <*>
                         o .: "street" <*>
                         o .: "city" <*>
                         o .: "state" <*>
                         o .: "zipcode" <*>
                         o .: "username" <*>
                         o .: "password"
  parseJSON _ = mzero


vendorTable :: O.Table VendorColWrite VendorColRead
vendorTable = O.Table "vendors" (pVendor Vendor { vendorId = O.optional "id"
                                          , vendorName = O.required "name"
                                          , vendorStreet = O.required "street"
                                          , vendorCity = O.required "city"
                                          , vendorState = O.required "state"
                                          , vendorZip = O.required "zip"
                                          , vendorUsername = O.required "username"
                                          , vendorPassword = O.required "password"
                                          })

vendorToPG :: VendorWrite -> IO VendorColWrite
vendorToPG v = do
  hashedPwd <- flip makePassword 12 . BS.pack . vendorPassword $ v
  return
    Vendor { vendorId = Nothing
           , vendorName = O.pgString . vendorName $ v
           , vendorStreet = O.pgString . vendorStreet $ v
           , vendorCity = O.pgString . vendorCity $ v
           , vendorState = O.pgString . vendorState $ v
           , vendorZip = O.pgString . vendorZip $ v
           , vendorUsername = O.pgString . vendorUsername $ v
           , vendorPassword = O.pgStrictByteString hashedPwd
           }

authVendor :: Maybe VendorRead -> VendorWrite -> Bool
authVendor Nothing _ = False
authVendor (Just dbVendor) attempt = verifyPassword (BS.pack . vendorPassword $ attempt)
                                                    (vendorPassword dbVendor)
