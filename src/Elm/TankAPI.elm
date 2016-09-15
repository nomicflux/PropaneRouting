module TankAPI exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import Task
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

get : Task.Task Http.Error (List (TankRead))
get =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/tanks"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeTankRead)
      (Http.send Http.defaultSettings request)

getById : Int -> Task.Task Http.Error (Maybe (TankRead))
getById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/tanks/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.maybe decodeTankRead)
      (Http.send Http.defaultSettings request)

getByHub : Int -> Task.Task Http.Error (List (TankRead))
getByHub hub =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/tanks/" ++ "hub"
          ++ "/" ++ (hub |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeTankRead)
      (Http.send Http.defaultSettings request)

type alias TankWrite =
  { tankId : Maybe Int
  , tankHub : Int
  , tankName : String
  , tankYellow : Maybe Int
  , tankRed : Maybe Int
  , tankLat : Float
  , tankLng : Float
  }

decodeTankWrite : Json.Decode.Decoder TankWrite
decodeTankWrite =
  Json.Decode.succeed TankWrite
    |: ("id" := Json.Decode.maybe Json.Decode.int)
    |: ("hub" := Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("yellow" := Json.Decode.maybe Json.Decode.int)
    |: ("red" := Json.Decode.maybe Json.Decode.int)
    |: ("lat" := Json.Decode.float)
    |: ("lng" := Json.Decode.float)

encodeTankWrite : TankWrite -> Json.Encode.Value
encodeTankWrite x =
  Json.Encode.object
    [ ( "id", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.tankId) )
    , ( "hub", Json.Encode.int x.tankHub )
    , ( "name", Json.Encode.string x.tankName )
    , ( "yellow", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.tankYellow) )
    , ( "red", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.tankRed) )
    , ( "lat", Json.Encode.float x.tankLat )
    , ( "lng", Json.Encode.float x.tankLng )
    ]

post : TankWrite -> Task.Task Http.Error (Maybe Int)
post body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/tanks"
      , body =
          Http.string (Json.Encode.encode 0 (encodeTankWrite body))
      }
  in
    Http.fromJson
      (Json.Decode.maybe Json.Decode.int)
      (Http.send Http.defaultSettings request)
