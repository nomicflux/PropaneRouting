{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Login where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (forM_)
import Data.Maybe (listToMaybe)
-- import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.Text.Encoding (encodeUtf8)
import qualified Opaleye as O
import Servant

import App
import Models.Login
import Models.Vendor
import Queries.Vendor

type CookieHeader = Header "JWT-Token" Text

type LoginAPI = "in" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[CookieHeader] Token)
  :<|> "out" :> Header "JWT-Token" Text :> Post '[JSON] (Headers '[CookieHeader] (Maybe Token))

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: ServerT LoginAPI AppM
loginServer = getToken
              :<|> deleteToken

getToken :: User -> AppM (Headers '[CookieHeader] Token)
getToken u = do
  con <- getConn
  s <- getSecret <$> ask
  addToLogger "[+] Logging In"
  dbVendor <- liftIO $ listToMaybe <$> O.runQuery con (vendorByUsernameQuery $ userName u)
  token <- liftIO $ grantToken s dbVendor
  addToLogger . show $ T.unpack . tokenText <$> token
  case dbVendor of
    Nothing -> addToLogger "Vendor not found" >> throwError (err401 { errBody = "Invalid username / password"})
    Just vendor -> case (authPassword vendor (userPassword u), token) of
      (True, Just t) -> do
        addToSession (vendorId vendor) (tokenText t)
        return (addHeader (tokenText t) t)
      _ -> addToLogger "Invalid Login" >> throwError (err401 { errBody = "Invalid username / password" })

deleteToken :: Maybe Text -> AppM (Headers '[CookieHeader] (Maybe Token))
deleteToken Nothing = return $ addHeader "" Nothing
deleteToken (Just token) = do
  addToLogger "[-] Logging Out"
  sec <- getSecret <$> ask
  let mvid = getVendor sec (Token token)
  forM_ mvid removeFromSession
  newToken <- liftIO $ expireToken (Token token)
  addToLogger $ T.unpack $ tokenText newToken
  return $ addHeader (tokenText newToken) (Just newToken)
