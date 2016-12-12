{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Server where

import qualified Servant as S
import Servant ((:<|>)( .. ), (:>), (:~>), Context( .. ))
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler, AuthServerData)
import Servant.API.Experimental.Auth (AuthProtect)
import qualified Network.Wai as Wai

import qualified Control.Concurrent.STM as STM
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.DateTime (getCurrentTime)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified System.Log.FastLogger as FL

import App (Config ( .. )
           , AppM
           , VendorID
           , EnvConfig ( .. )
           , checkVendor
           )
import Api.Hub
import Api.Tank
import Api.Reading
import Api.Vendor
import Api.Login
import Models.Login


readerTToExcept :: Config -> AppM :~> ExceptT S.ServantErr IO
readerTToExcept pool = S.Nat (`runReaderT` pool)

type APIWeb = "hubs" :> (HubAPIGet :<|> HubAPIPost)
              :<|> "tanks" :> (TankAPIGet :<|> TankAPIPost)
              :<|> "readings" :> ReadingAPIGet

type APISensor = "readings" :> ReadingAPIPost

type AuthAPI = APIWeb

type UnAuthAPI = "vendors" :> VendorAPI
                 :<|> "post" :> APISensor
                 :<|> "log" :> LoginAPI
                 :<|> S.Raw

serverWeb :: VendorID -> S.ServerT APIWeb AppM
serverWeb v = (hubGetServer v :<|> hubPostServer)
              :<|> (tankGetServer v :<|> tankPostServer)
              :<|> readingGetServer v

serverSensor :: S.ServerT APISensor AppM
serverSensor = readingPostServer

type FullAPI = "auth" :> AuthProtect "jwt-auth" :> AuthAPI
               :<|> UnAuthAPI

fullApi :: S.Proxy FullAPI
fullApi = S.Proxy

type VendorAuth = AuthHandler Wai.Request VendorID

authHandler :: Config -> VendorAuth
authHandler cfg =
  let handler req = case lookup "JWT-Token" (Wai.requestHeaders req) of
        Nothing -> S.throwError (S.err401 { S.errBody = "Missing auth header" })
        Just cookie -> do
          -- liftIO $ FL.pushLogStrLn (getLogger cfg) $ FL.toLogStr (show cookie)
          let token = Token $ decodeUtf8 cookie
              secret = getSecret cfg
              mvid = getVendor secret token
              mtime = getTime secret token
          case (mvid, mtime) of
            (Nothing, _) -> S.throwError (S.err401 { S.errBody = "No user in token" })
            (_, Nothing) -> S.throwError (S.err401 { S.errBody = "No user in token" })
            (Just vid, Just expires) -> do
              now <- liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime
              unless (expires > now) $ S.throwError (S.err401 { S.errBody = "Expired token" })
              let session = getSession cfg
              check <- liftIO $ STM.atomically $ checkVendor session vid (tokenText token)
              liftIO $ FL.pushLogStrLn (getLogger cfg) $ FL.toLogStr (show check)
              if check then return vid
                else S.throwError (S.err401 { S.errBody = "No record of token" })
  in mkAuthHandler handler

type instance AuthServerData (AuthProtect "jwt-auth") = VendorID

authContext :: Config -> Context (VendorAuth ': '[])
authContext cfg = authHandler cfg :. EmptyContext

fullApp :: EnvConfig -> Config -> Wai.Application
fullApp _ cfg =
  S.serveWithContext fullApi (authContext cfg) $
  (S.enter (readerTToExcept cfg) . serverWeb)
  :<|> (S.enter (readerTToExcept cfg) vendorServer
        :<|> S.enter (readerTToExcept cfg) serverSensor
        :<|> S.enter (readerTToExcept cfg) loginServer
        :<|> S.serveDirectory "public")
