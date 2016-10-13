{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Login where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
-- import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.Text.Encoding (encodeUtf8)
import qualified Opaleye as O
import Servant
import Web.JWT (Secret)

import App
import Models.Login
import Models.Vendor
import Queries.Vendor

type CookieHeader = Header "Send-Cookie" Text

type LoginAPI = "in" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[CookieHeader] Token)
  :<|> "out" :> Header "Cookie" Token :> Post '[JSON] (Headers '[CookieHeader] (Maybe Token))

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: Secret -> ServerT LoginAPI AppM
loginServer s = getToken s
                :<|> deleteToken

getToken :: Secret -> User -> AppM (Headers '[CookieHeader] Token)
getToken s u = do
  con <- getConn
  addToLogger "[+] Logging In"
  dbVendor <- liftIO $ listToMaybe <$> O.runQuery con (vendorByUsernameQuery $ userName u)
  token <- liftIO $ grantToken s dbVendor
  addToLogger . show $ T.unpack . tokenToCookie <$> token
  case ((flip authPassword $ userPassword u) <$> dbVendor, token) of
    (Just True, Just t) -> return (addHeader (tokenToCookie t) t)
    _ -> addToLogger "Invalid Login" >> throwError (err401 { errBody = "Invalid username / password" })

deleteToken :: Maybe Token -> AppM (Headers '[CookieHeader] (Maybe Token))
deleteToken Nothing = return $ addHeader "" Nothing
deleteToken (Just token) = do
  addToLogger "[-] Logging Out"
  newToken <- liftIO $ expireToken token
  addToLogger $ T.unpack $ tokenToCookie newToken
  return $ addHeader (tokenToCookie newToken) (Just newToken)
