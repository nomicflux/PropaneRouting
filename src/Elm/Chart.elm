module Chart exposing ( addReadings
                      , clearChart
                      , toggleManual
                      , chartToLevel
                      , getRange
                      , showDate
                      , color
                      , chartLocation
                      , Chart
                      )

import SharedModels exposing (..)
import ReadingAPI exposing (ReadingRead)
import Date exposing (Date, year, month, day, hour, minute)

type alias Chart = { id : ChartID
                   , pos : GMPos
                   , values : List ChartPt
                   , numValues : Int
                   , yellow : Float
                   , red : Float
                   , lastPulled : Maybe Int
                   , earliestDate : Maybe Date
                   , latestDate : Maybe Date
                   , manual : Bool
                   }

addReadings : ChartID -> List ReadingRead -> Chart -> Chart
addReadings id vals chart =
    if chart.id /= id
        then chart
        else
            let earliestReading = List.head vals
                reversed = List.reverse vals
                latestReading = List.head reversed
                newVals = List.map (\v -> { x = v.readingSensorSent
                                          , y = v.readingValue})
                          reversed
                lastPulled = Maybe.map .readingId latestReading
                earliestDate = Maybe.map .readingDbReceived earliestReading
                latestDate = Maybe.map .readingDbReceived latestReading
            in { chart | values =  newVals ++ chart.values
               , numValues = chart.numValues + List.length vals
               , lastPulled = Maybe.oneOf [lastPulled, chart.lastPulled]
               , latestDate = Maybe.oneOf [latestDate, chart.latestDate]
               , earliestDate = Maybe.oneOf [chart.earliestDate, earliestDate]
               }

clearChart : Chart -> Chart
clearChart chart =
    { chart | values = []
    , numValues = 0
    , lastPulled = Nothing
    , earliestDate = Nothing
    , latestDate = Nothing
    }

toggleManual : ChartID -> Chart -> Chart
toggleManual cid chart =
    if chart.id == cid
    then { chart | manual = not chart.manual }
    else chart

empty : Float
empty = 1.0

takeAvg : Int
takeAvg = 5

chartToLevel : Chart -> (ChartID, Level)
chartToLevel chart =
    let size = chart.numValues
        back = Basics.max 0.0 (size - takeAvg - 1 |> toFloat)
        numTaken = (toFloat size - back)
        latestFive = List.take (round numTaken) chart.values
        avgVal = (List.foldr (\pt acc -> pt.y + acc) 0 latestFive) / numTaken
        lvl = if round numTaken == 0 then "noreadings"
              else if avgVal <= empty then "empty"
                   else if avgVal <= chart.red then "red"
                        else if avgVal <= chart.yellow then "yellow"
                             else "green"
    in (chart.id, lvl)

getRange : List Float -> (Float, Float)
getRange arr =
    let (minval, maxval) = List.foldl (\x (cmin, cmax) ->
                                        let nmin = Basics.min cmin x
                                            nmax = Basics.max cmax x
                                        in (nmin, nmax)
                                   ) (1/0, -1/0) arr
        retMin = if isInfinite minval then 0 else minval
        retMax = if isInfinite maxval then 0 else maxval
    in (retMin, retMax)

color : Float -> Float -> Float -> String
color red yellow val =
    if val <= yellow then "green"
    else if val <= red then "yellow"
         else "red"

minLen : Int -> String
minLen x = if x < 10 then "0" ++ toString x else toString x

showDate : Date -> String
showDate date =
    let hr = hour date |> minLen
        mn = minute date |> minLen
        yr = year date |> toString
        mt = month date |> toString
        dy = day date |> toString
    in yr ++ "-" ++ mt ++ "-" ++ dy ++ " " ++ hr ++ ":" ++ mn

chartLocation : Chart -> String
chartLocation chart = toString chart.id
