module Generated.TankAPI exposing (..)

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
  , tankYellow : Int
  , tankRed : Int
  , tankLat : Float
  , tankLng : Float
  }

decodeTankRead : Json.Decode.Decoder TankRead
decodeTankRead =
  Json.Decode.succeed TankRead
    |: ("tankId" := Json.Decode.int)
    |: ("tankHub" := Json.Decode.int)
    |: ("tankName" := Json.Decode.string)
    |: ("tankYellow" := Json.Decode.int)
    |: ("tankRed" := Json.Decode.int)
    |: ("tankLat" := Json.Decode.float)
    |: ("tankLng" := Json.Decode.float)


encodeTankRead : TankRead -> Json.Encode.Value
encodeTankRead x =
  Json.Encode.object
    [ ( "tankId", Json.Encode.int x.tankId )
    , ( "tankHub", Json.Encode.int x.tankHub )
    , ( "tankName", Json.Encode.string x.tankName )
    , ( "tankYellow", Json.Encode.int x.tankYellow )
    , ( "tankRed", Json.Encode.int x.tankRed )
    , ( "tankLat", Json.Encode.float x.tankLat )
    , ( "tankLng", Json.Encode.float x.tankLng )
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

getHubByHub : Int -> Task.Task Http.Error (List (TankRead))
getHubByHub hub =
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
    |: ("tankId" := Json.Decode.maybe Json.Decode.int)
    |: ("tankHub" := Json.Decode.int)
    |: ("tankName" := Json.Decode.string)
    |: ("tankYellow" := Json.Decode.maybe Json.Decode.int)
    |: ("tankRed" := Json.Decode.maybe Json.Decode.int)
    |: ("tankLat" := Json.Decode.float)
    |: ("tankLng" := Json.Decode.float)

encodeTankWrite : TankWrite -> Json.Encode.Value
encodeTankWrite x =
  Json.Encode.object
    [ ( "tankId", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.tankId) )
    , ( "tankHub", Json.Encode.int x.tankHub )
    , ( "tankName", Json.Encode.string x.tankName )
    , ( "tankYellow", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.tankYellow) )
    , ( "tankRed", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.tankRed) )
    , ( "tankLat", Json.Encode.float x.tankLat )
    , ( "tankLng", Json.Encode.float x.tankLng )
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
