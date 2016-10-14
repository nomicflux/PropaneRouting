{-# LANGUAGE OverloadedStrings #-}

module Models.Login where

import Prelude hiding (exp)
import Control.Monad (mzero)
-- import Data.Default.Class (def)
import Data.Aeson
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.DateTime (DateTime, addMinutes, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import qualified Data.Map as M
-- import Data.Monoid ((<>))
import Data.Text (Text)
-- import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BS
-- import Web.HttpApiData
import Web.JWT

import App
import Models.Vendor

data User = User { userName :: String
                 , userPassword :: String
                 }

instance FromJSON User where
  parseJSON (Object o) = User <$>
    o .: "username" <*>
    o .: "password"
  parseJSON _ = mzero

data Token = Token { tokenText :: Text }

instance ToJSON Token where
  toJSON token = object [ "token" .= tokenText token ]

instance FromJSON Token where
  parseJSON (Object o) = Token <$>
    o .: "token"
  parseJSON _ = mzero

-- cleanWhitespace :: String -> String
-- cleanWhitespace = dropWhile (== ' ')

-- cleanSemi :: String -> String
-- cleanSemi s = if last s == ';' then init s else s

-- keyValPair :: String -> Maybe (String, String)
-- keyValPair s =
--   case span (/= '=') s of
--     (x@(_:_), _:ys) -> Just (x, cleanSemi ys)
--     (x@(_:_), []) -> Just (x, "")
--     _ -> Nothing

-- mkPairs :: String -> [String]
-- mkPairs s =
--   case span (/= ';') s of
--     (x@(_:_), _:ys) -> cleanWhitespace x : mkPairs ys
--     (x@(_:_), []) -> [cleanWhitespace x]
--     _ -> []

-- instance FromHttpApiData Token where
--   parseHeader bstoken = _
    -- let
    --   mkvs = traverse keyValPair (mkPairs . BS.unpack $ bstoken)
    -- in
    --   case mkvs of
    --     Nothing -> Left "Parse error"
    --     Just kvs ->
    --       let kvmap = M.fromList kvs
    --           mtoken = Token <$>
    --             (T.pack <$> M.lookup "token" kvmap) <*>
    --             (M.lookup "Expires" kvmap >>= parseExpiration)
    --       in case mtoken of
    --         Nothing -> Left "Missing fields"
    --         Just t -> Right t
  -- parseQueryParam _ = Left "Not implemented yet"

-- instance ToHttpApiData Token where
--   toHeader = encodeUtf8 . tokenText
--   toQueryParam = tokenText

expirationFormat :: String
expirationFormat = "%a, %d %b %0Y %H:%M:%S %Z"

formatExpiration :: DateTime -> Text
formatExpiration =
  T.pack . formatTime defaultTimeLocale expirationFormat

parseExpiration :: String -> Maybe DateTime
parseExpiration =
  parseTimeM True defaultTimeLocale expirationFormat

getVendor :: Secret -> Token -> Maybe VendorID
getVendor s token = do
  jwt <- decodeAndVerifySignature s (tokenText token)
  val <- M.lookup "vendor" . unregisteredClaims . claims $ jwt
  case fromJSON val of
    Error _ -> Nothing
    Success vid -> Just vid

mkToken :: Secret -> VendorID -> IO Token
mkToken s vid = do
  now <- getCurrentTime
  let expires = addMinutes 60 now
      mdate = numericDate $ utcTimeToPOSIXSeconds expires
      stuff = def { exp = mdate
                  , unregisteredClaims = M.singleton "vendor" (toJSON vid)
                  }
      text = encodeSigned HS256 s stuff
  return Token { tokenText = text }

expireToken :: Token -> IO Token
expireToken token = do
  -- now <- getCurrentTime
  -- let expires = addMinutes (-60) now
  return $ token { tokenText = "" }

grantToken :: Secret -> Maybe VendorRead -> IO (Maybe Token)
grantToken _ Nothing = return Nothing
grantToken s (Just vendor) =
  Just <$> mkToken s (vendorId vendor)
