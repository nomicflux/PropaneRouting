{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Login where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
-- import Data.Monoid ((<>))
import Data.Text (Text)
-- import Data.Text.Encoding (encodeUtf8)
import qualified Opaleye as O
import Servant
import Web.JWT (Secret)

import App
import Models.Login
import Models.Vendor
import Queries.Vendor

type CookieHeader = Header "Send-Cookie" Text

type LoginAPI = ReqBody '[JSON] User :> Post '[JSON] (Headers '[CookieHeader] Token)

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: Secret -> ServerT LoginAPI AppM
loginServer = getToken

getToken :: Secret -> User -> AppM (Headers '[CookieHeader] Token)
getToken s u = do
  con <- getConn
  dbVendor <- liftIO $ listToMaybe <$> O.runQuery con (vendorByUsernameQuery $ userName u)
  case ((flip authPassword $ userPassword u) <$> dbVendor, grantToken s dbVendor) of
    (Just True, Just t) -> return (addHeader (tokenText t) t)
    _ -> addToLogger "Invalid Login" >> throwError (err401 { errBody = "Invalid username / password" })
