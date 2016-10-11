{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Login where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (listToMaybe)
import qualified Opaleye as O
import Servant
import Web.JWT (Secret)

import App
import Models.Login
import Models.Vendor
import Queries.Vendor

type LoginAPI = ReqBody '[JSON] User :> Get '[JSON] Token

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: Secret -> ServerT LoginAPI AppM
loginServer s = getToken s

getToken :: Secret -> User -> AppM Token
getToken s u = do
  con <- getConn
  dbVendor <- liftIO $ listToMaybe <$> O.runQuery con (vendorByUsernameQuery $ userName u)
  case ((flip authPassword $ userPassword u) <$> dbVendor, grantToken s dbVendor) of
    (Just True, Just t) -> return t
    _ -> lift $ throwE err401
