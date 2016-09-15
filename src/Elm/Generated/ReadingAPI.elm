module Generated.ReadingAPI exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
-- import String
import Task
import Exts.Date exposing(toISOString)
import Date exposing (Date)


type alias ReadingRead =
  { readingId : Int
  , readingTank : Int
  , readingValue : Int
  , readingDbReceived : Date
  , readingSensorSent : Date
  }

decodeReadingRead : Json.Decode.Decoder ReadingRead
decodeReadingRead =
  Json.Decode.succeed ReadingRead
    |: ("id" := Json.Decode.int)
    |: ("tank" := Json.Decode.int)
    |: ("value" := Json.Decode.int)
    |: ("dbreceived" := Json.Decode.Extra.date)
    |: ("sensorsent" := Json.Decode.Extra.date)


encodeReadingRead : ReadingRead -> Json.Encode.Value
encodeReadingRead x =
  Json.Encode.object
    [ ( "id", Json.Encode.int x.readingId )
    , ( "tank", Json.Encode.int x.readingTank )
    , ( "value", Json.Encode.int x.readingValue )
    , ( "dbreceived", (Json.Encode.string << Exts.Date.toISOString) x.readingDbReceived )
    , ( "sensorsent", (Json.Encode.string << Exts.Date.toISOString) x.readingSensorSent )
    ]

get : Task.Task Http.Error (List (ReadingRead))
get =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/readings"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send Http.defaultSettings request)

getById : Int -> Task.Task Http.Error (Maybe (ReadingRead))
getById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/readings/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.maybe decodeReadingRead)
      (Http.send Http.defaultSettings request)

getReadingByTank : Int -> Task.Task Http.Error (List (ReadingRead))
getReadingByTank tank =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/readings/" ++ "tank"
          ++ "/" ++ (tank |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send Http.defaultSettings request)

getReadingByHub : Int -> Task.Task Http.Error (List (ReadingRead))
getReadingByHub hub =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/readings/" ++ "hub"
          ++ "/" ++ (hub |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send Http.defaultSettings request)

getHubYellow : Int -> Task.Task Http.Error (List (ReadingRead))
getHubYellow yellow =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/readings/" ++ "yellow"
          ++ "/" ++ (yellow |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send Http.defaultSettings request)

getHubRed : Int -> Task.Task Http.Error (List (ReadingRead))
getHubRed red =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/readings/" ++ "red"
          ++ "/" ++ (red |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send Http.defaultSettings request)

type alias ReadingWrite =
  { readingId : Maybe Int
  , readingTank : Int
  , readingValue : Int
  , readingDbReceived : Maybe Date
  , readingSensorSent : Date
  }

decodeReadingWrite : Json.Decode.Decoder ReadingWrite
decodeReadingWrite =
  Json.Decode.succeed ReadingWrite
    |: ("id" := Json.Decode.maybe Json.Decode.int)
    |: ("tank" := Json.Decode.int)
    |: ("value" := Json.Decode.int)
    |: ("dbreceived" := Json.Decode.maybe Json.Decode.Extra.date)
    |: ("sensorsent" := Json.Decode.Extra.date)

encodeReadingWrite : ReadingWrite -> Json.Encode.Value
encodeReadingWrite x =
  Json.Encode.object
    [ ( "id", Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int x.readingId) )
    , ( "tank", Json.Encode.int x.readingTank )
    , ( "value", Json.Encode.int x.readingValue )
    , ( "dbreceived", Maybe.withDefault Json.Encode.null (Maybe.map (Json.Encode.string << Exts.Date.toISOString) x.readingDbReceived) )
    , ( "sensorsent", (Json.Encode.string << Exts.Date.toISOString) x.readingSensorSent)
    ]

post : ReadingWrite -> Task.Task Http.Error (Maybe Int)
post body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/readings"
      , body =
          Http.string (Json.Encode.encode 0 (encodeReadingWrite body))
      }
  in
    Http.fromJson
      (Json.Decode.maybe Json.Decode.int)
      (Http.send Http.defaultSettings request)
