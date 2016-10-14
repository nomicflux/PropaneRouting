module ReadingAPI exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
-- import String
import Task
import Exts.Date exposing(toISOString)
import Date exposing (Date)
import Maybe exposing (Maybe)
import Token

dset : Http.Settings
dset = Http.defaultSettings

tlsSettings : Http.Settings
tlsSettings = { dset | withCredentials = True }

type alias ReadingRead =
  { readingId : Int
  , readingTank : Int
  , readingValue : Float
  , readingDbReceived : Date
  , readingSensorSent : Float
  }

decodeReadingRead : Json.Decode.Decoder ReadingRead
decodeReadingRead =
  Json.Decode.succeed ReadingRead
    |: ("id" := Json.Decode.int)
    |: ("tank" := Json.Decode.int)
    |: ("value" := Json.Decode.float)
    |: ("dbreceived" := Json.Decode.Extra.date)
    |: ("sensorsent" := Json.Decode.map (Date.toTime >> \x -> x / 1000) Json.Decode.Extra.date)


encodeReadingRead : ReadingRead -> Json.Encode.Value
encodeReadingRead x =
  Json.Encode.object
    [ ( "id", Json.Encode.int x.readingId )
    , ( "tank", Json.Encode.int x.readingTank )
    , ( "value", Json.Encode.float x.readingValue )
    , ( "dbreceived", (Json.Encode.string << Exts.Date.toISOString) x.readingDbReceived )
    , ( "sensorsent", (Json.Encode.string << Exts.Date.toISOString << Date.fromTime << \x -> x * 1000) x.readingSensorSent )
    ]

get : Maybe Token.Token -> Task.Task Http.Error (List (ReadingRead))
get token =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/readings"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send tlsSettings request)

getById : Maybe Token.Token -> Int -> Task.Task Http.Error (Maybe (ReadingRead))
getById token id =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/readings/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.maybe decodeReadingRead)
      (Http.send tlsSettings request)

getByTank : Maybe Token.Token -> Int -> Maybe Int -> Maybe Int -> Task.Task Http.Error (List (ReadingRead))
getByTank token tank mseconds mreading =
  let
    queries = case (mseconds, mreading) of
                  (Nothing, Nothing) -> ""
                  (Nothing, Just rid) -> "?lastreading=" ++ toString rid
                  (Just sec, Nothing) -> "?seconds=" ++ toString sec
                  (Just sec, Just rid) -> "?seconds=" ++ toString sec ++ "&lastreading=" ++ toString rid

    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/readings/" ++ "tank"
          ++ "/" ++ (tank |> toString |> Http.uriEncode) ++ (queries)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send tlsSettings request)

getByHub : Maybe Token.Token -> Int -> Task.Task Http.Error (List (ReadingRead))
getByHub token hub =
  let
    request =
      { verb =
          "GET"
      , headers =
          ("Content-Type", "application/json") :: Token.addHeader token
      , url =
          "/auth/readings/" ++ "hub"
          ++ "/" ++ (hub |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeReadingRead)
      (Http.send tlsSettings request)
