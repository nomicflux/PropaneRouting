module LoginAPI exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
-- import String
import Task
import Debug

dset : Http.Settings
dset = Http.defaultSettings

tlsSettings : Http.Settings
tlsSettings = { dset | withCredentials = True }


type alias Token =
    { tokenText : String
    , tokenExpires : String
    }

type alias User =
    { username : String
    , password : String
    }

decodeToken : Json.Decode.Decoder Token
decodeToken =
    Json.Decode.succeed Token
        |: ("token" := Json.Decode.string)
        |: ("expires" := Json.Decode.string)

encodeUser : User -> Json.Encode.Value
encodeUser user =
    Json.Encode.object
        [ ("username", Json.Encode.string user.username)
        , ("password", Json.Encode.string user.password)
        ]

login : User -> Task.Task Http.Error (Maybe Token)
login user =
    let
        request =
            { verb = "POST"
            , headers = [("Content-Type", "application/json")]
            , url = "/log/in"
            , body = Http.string (Json.Encode.encode 0 (encodeUser user)) |> Debug.log "Logging In"
            }
    in
        Http.fromJson
            (Json.Decode.maybe decodeToken)
            (Http.send tlsSettings request)

logout : Task.Task Http.Error (Maybe Token)
logout =
    let
        request =
            { verb = "POST"
            , headers = [("Content-Type", "application/json")]
            , url = "/log/out"
            , body = Http.string (Json.Encode.encode 0 (Json.Encode.string "")) |> Debug.log "Logging Out"
            }
    in
        Http.fromJson
            (Json.Decode.maybe decodeToken)
            (Http.send tlsSettings request)
