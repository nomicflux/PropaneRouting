module HubAPI exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import Task
import Maybe exposing (Maybe)
import Token

type alias HubRead =
  { hubId : Int
  , hubName : String
  , hubLat : Float
  , hubLng : Float
  }

decodeHubRead : Json.Decode.Decoder HubRead
decodeHubRead =
  Json.Decode.succeed HubRead
    |: ("id" := Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("lat" := Json.Decode.float)
    |: ("lng" := Json.Decode.float)

encodeHubRead : HubRead -> Json.Encode.Value
encodeHubRead x =
  Json.Encode.object
    [ ( "id", Json.Encode.int x.hubId )
    , ( "name", Json.Encode.string x.hubName )
    , ( "lat", Json.Encode.float x.hubLat )
    , ( "lng", Json.Encode.float x.hubLng )
    ]

get : Maybe Token.Token -> Task.Task Http.Error (List (HubRead))
get token =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/hubs"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeHubRead)
      (Http.send Http.defaultSettings request)

getById : Maybe Token.Token -> Int -> Task.Task Http.Error (Maybe HubRead)
getById token id =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/hubs/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.maybe decodeHubRead)
      (Http.send Http.defaultSettings request)
