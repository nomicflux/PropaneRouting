{-# LANGUAGE OverloadedStrings #-}

module Models.Login where

import Control.Monad (mzero)
-- import Data.Default.Class (def)
import Data.Aeson
import Data.DateTime (DateTime, addMinutes, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Web.HttpApiData
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
                   , tokenExpires :: DateTime
                   }

tokenToCookie :: Token -> Text
tokenToCookie token =
  "token=" <> tokenText token <> "; expires=" <> formatExpiration (tokenExpires token)

instance ToJSON Token where
  toJSON token = object [ "token" .= tokenText token
                        , "expires" .= tokenExpires token]

instance FromJSON Token where
  parseJSON (Object o) = Token <$> o .: "token" <*> o .: "expires"
  parseJSON _ = mzero

cleanWhitespace :: String -> String
cleanWhitespace = dropWhile (== ' ')

cleanSemi :: String -> String
cleanSemi s = if last s == ';' then init s else s

keyValPair :: String -> Maybe (String, String)
keyValPair s =
  case span (/= '=') s of
    (x@(_:_), _:ys) -> Just (cleanWhitespace x, cleanSemi . cleanWhitespace $ ys)
    _ -> Nothing

mkPairs :: String -> [String]
mkPairs s =
  case span (/= ';') s of
    (x@(_:_), _:ys) -> x : mkPairs ys
    (x@(_:_), []) -> [x]
    _ -> []

instance FromHttpApiData Token where
  parseHeader bstoken =
    let
      mkvs = traverse keyValPair (mkPairs . BS.unpack $ bstoken)
    in
      case mkvs of
        Nothing -> Left "Parse error"
        Just kvs ->
          let kvmap = M.fromList kvs
              mtoken = Token <$>
                (T.pack <$> M.lookup "token" kvmap) <*>
                (M.lookup "expires" kvmap >>= parseExpiration)
          in case mtoken of
            Nothing -> Left "Missing fields"
            Just t -> Right t
  parseQueryParam _ = Left "Not implemented yet"

expirationFormat :: String
expirationFormat = "%a, %d %b %0Y %H:%M:%S %Z"

formatExpiration :: DateTime -> Text
formatExpiration =
  T.pack . formatTime defaultTimeLocale expirationFormat

parseExpiration :: String -> Maybe DateTime
parseExpiration =
  parseTimeM True defaultTimeLocale expirationFormat

mkToken :: Text -> IO Token
mkToken text = do
  now <- getCurrentTime
  let expires = addMinutes 60 now
  return Token { tokenText = text
               , tokenExpires = expires
               }

expireToken :: Token -> IO Token
expireToken token = do
  now <- getCurrentTime
  let expires = addMinutes (-60) now
  return $ token { tokenExpires = expires, tokenText = "" }

grantToken :: Secret -> Maybe VendorRead -> IO (Maybe Token)
grantToken _ Nothing = return Nothing
grantToken s (Just _) = do
  let stuff = def
      text = encodeSigned HS256 s stuff
  Just <$> mkToken text
