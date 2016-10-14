module TankAPI exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import Task
import Maybe exposing (Maybe)
import Token
-- import String exposing (toInt)
-- import Result exposing (toMaybe)
-- import Debug exposing (crash)
-- import Exts.Date exposing(toISOString)
-- import Date exposing (Date)


type alias TankRead =
  { tankId : Int
  , tankHub : Int
  , tankName : String
  , tankYellow : Float
  , tankRed : Float
  , tankLat : Float
  , tankLng : Float
  }

decodeTankRead : Json.Decode.Decoder TankRead
decodeTankRead =
  Json.Decode.succeed TankRead
    |: ("id" := Json.Decode.int)
    |: ("hub" := Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("yellow" := Json.Decode.float)
    |: ("red" := Json.Decode.float)
    |: ("lat" := Json.Decode.float)
    |: ("lng" := Json.Decode.float)


encodeTankRead : TankRead -> Json.Encode.Value
encodeTankRead x =
  Json.Encode.object
    [ ( "id", Json.Encode.int x.tankId )
    , ( "hub", Json.Encode.int x.tankHub )
    , ( "name", Json.Encode.string x.tankName )
    , ( "yellow", Json.Encode.float x.tankYellow )
    , ( "red", Json.Encode.float x.tankRed )
    , ( "lat", Json.Encode.float x.tankLat )
    , ( "lng", Json.Encode.float x.tankLng )
    ]

get : Maybe Token.Token -> Task.Task Http.Error (List (TankRead))
get token =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/tanks"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeTankRead)
      (Http.send Http.defaultSettings request)

getById : Maybe Token.Token -> Int -> Task.Task Http.Error (Maybe (TankRead))
getById token id =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/tanks/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.maybe decodeTankRead)
      (Http.send Http.defaultSettings request)

getByHub : Maybe Token.Token -> Int -> Task.Task Http.Error (List (TankRead))
getByHub token hub =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/tanks/" ++ "hub"
          ++ "/" ++ (hub |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeTankRead)
      (Http.send Http.defaultSettings request)
