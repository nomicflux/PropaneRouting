module Generated.HubAPI exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import Task


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

get : Task.Task Http.Error (List (HubRead))
get =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/hubs"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeHubRead)
      (Http.send Http.defaultSettings request)

getById : Int -> Task.Task Http.Error (Maybe HubRead)
getById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/hubs/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.maybe decodeHubRead)
      (Http.send Http.defaultSettings request)

type alias HubWrite =
  { hubId : Maybe Int
  , hubName : String
  , hubLat : Float
  , hubLng : Float
  }

decodeHubWrite : Json.Decode.Decoder HubWrite
decodeHubWrite =
  Json.Decode.succeed HubWrite
    |: ("id" := Json.Decode.maybe Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("lat" := Json.Decode.float)
    |: ("lng" := Json.Decode.float)

encodeHubWrite : HubWrite -> Json.Encode.Value
encodeHubWrite x =
  Json.Encode.object
    [ ( "id", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.hubId) )
    , ( "name", Json.Encode.string x.hubName )
    , ( "lat", Json.Encode.float x.hubLat )
    , ( "lng", Json.Encode.float x.hubLng )
    ]

post : HubWrite -> Task.Task Http.Error (Maybe Int)
post body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/hubs"
      , body =
          Http.string (Json.Encode.encode 0 (encodeHubWrite body))
      }
  in
    Http.fromJson
      (Json.Decode.maybe Json.Decode.int)
      (Http.send Http.defaultSettings request)
