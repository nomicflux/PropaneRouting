{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Vendor where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)

import App
import Models.Vendor
import Queries.Vendor

type VendorAPI = Get '[JSON] [VendorRead]
                 :<|> "id" :> Capture "id" VendorID :> Get '[JSON] (Maybe VendorRead)
                 :<|> "username" :> Capture "username" Username :> Get '[JSON] (Maybe VendorRead)
                 :<|> "verify" :> ReqBody '[JSON] VendorWrite :> Post '[JSON] Bool
                 :<|> ReqBody '[JSON] VendorWrite :> Post '[JSON] (Maybe VendorID)

vendorAPI :: Proxy VendorAPI
vendorAPI = Proxy

vendorServer :: ServerT VendorAPI AppM
vendorServer = getVendors
        :<|> getVendorById
        :<|> getVendorByUsername
        :<|> verifyVendor
        :<|> postVendor

getVendors :: AppM [VendorRead]
getVendors = do con <- getConn
                liftIO $ O.runQuery con vendorsQuery

getVendorById :: VendorID -> AppM (Maybe VendorRead)
getVendorById id' = do con <- getConn
                       liftIO $ listToMaybe <$> O.runQuery con (vendorByIdQuery id')

getVendorByUsername :: Username -> AppM (Maybe VendorRead)
getVendorByUsername uname = do con <- getConn
                               liftIO $
                                 listToMaybe <$> O.runQuery con (vendorByUsernameQuery uname)

verifyVendor :: VendorWrite -> AppM Bool
verifyVendor v = do dbVendor <- getVendorByUsername (vendorUsername v)
                    return $ authVendor dbVendor v

-- Change to notify if username already taken
postVendor :: VendorWrite -> AppM (Maybe VendorID)
postVendor v = do con <- getConn
                  newVendor <- liftIO $ vendorToPG v
                  liftIO $ listToMaybe <$>
                    O.runInsertManyReturning con vendorTable [newVendor] vendorId
