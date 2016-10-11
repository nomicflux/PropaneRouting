{-# LANGUAGE OverloadedStrings #-}

module Models.Login where

import Control.Monad (mzero)
-- import Data.Default.Class (def)
import Data.Aeson
import Data.Text (Text)
import Web.JWT

-- import App
import Models.Vendor

data User = User { userName :: String
                 , userPassword :: String
                 }

instance FromJSON User where
  parseJSON (Object o) = User <$>
    o .: "username" <*>
    o .: "password"
  parseJSON _ = mzero

data Token = Token { tokenText :: Text
                   }

instance ToJSON Token where
  toJSON (Token t) = object [ "token" .= t ]

instance FromJSON Token where
  parseJSON (Object o) = Token <$> o .: "token"
  parseJSON _ = mzero

grantToken :: Secret -> Maybe VendorRead -> Maybe Token
grantToken _ Nothing = Nothing
grantToken s (Just _) =
  let stuff = def
  in Just $ Token $ encodeSigned HS256 s stuff
