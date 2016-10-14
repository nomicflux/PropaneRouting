module Token exposing ( .. )

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Maybe exposing (Maybe(..))

type alias Token =
    { tokenText : String
    }

decodeToken : Json.Decode.Decoder Token
decodeToken =
    Json.Decode.succeed Token
        |: ("token" := Json.Decode.string)

encodeToken : Token -> Json.Encode.Value
encodeToken token =
    Json.Encode.object
        [ ("token", Json.Encode.string token.tokenText)]

tokenToCookie : Token -> String
tokenToCookie token = token.tokenText

addHeader : Maybe Token -> List (String, String)
addHeader mtoken =
    case mtoken of
        Nothing -> []
        Just token -> [("JWT-Token", tokenToCookie token)]
