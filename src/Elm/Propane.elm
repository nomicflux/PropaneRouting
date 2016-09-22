module Propane exposing (..)

import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
-- import Result exposing (Result)
import Svg
import Svg.Attributes as S
import SharedModels exposing (..)
import GMPorts exposing (..)
import Task
import Http
import Debug
import Time exposing (Time, second, millisecond)
import Date exposing (Date, year, month, day, hour, minute)
import HubAPI as Hub
import TankAPI as Tank
import ReadingAPI as Reading

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
type alias Model = { charts : List Chart
                   , numCharts : Int
                   , currChart : Maybe ChartID
                   , timeScale : Int
                   , includeNoReadings : Bool
                   }

type Msg = ClearChart ChartID
         | AddChart (ChartID, GMPos, Float, Float)
         | MarkerClicked ChartID
         | AddHub (Maybe Hub.HubRead)
         | AddTanks (List Tank.TankRead)
         | AddReadings ChartID (List Reading.ReadingRead)
         | Failure Http.Error
         | ChangeScale String
         | ToggleManual ChartID
         | ToggleNoReadings
         | RouteRed
         | RouteYellow
         | FastTick Time
         | SlowTick Time

initialModel : Model
initialModel = { charts = [ ]
               , numCharts = 0
               , currChart = Nothing
               , timeScale = 12
               , includeNoReadings = False
               }

addReadings : ChartID -> List Reading.ReadingRead -> Chart -> Chart
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
                lastPulled = Maybe.map (\r -> r.readingId) latestReading
                earliestDate = Maybe.map (\r -> r.readingDbReceived) earliestReading
                latestDate = Maybe.map (\r -> r.readingDbReceived) latestReading
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClearChart idx ->
            let newCharts = List.map (\c -> if c.id == idx then clearChart c else c) model.charts
            in ({ model | charts = newCharts}, Cmd.none )
        AddChart (id, pos, yellow, red) ->
            let newChart = { id = id
                           , pos = pos
                           , values = []
                           , numValues = 0
                           , yellow = yellow
                           , red = red
                           , lastPulled = Nothing
                           , earliestDate = Nothing
                           , latestDate = Nothing
                           , manual = False
                           }
                newCt = model.numCharts + 1
            in ( { model | charts = newChart :: model.charts, numCharts = newCt }, Cmd.none)
        MarkerClicked pos ->
            ( { model | currChart = Just pos }, Cmd.none )
        AddHub mhub ->
            case mhub of
                Nothing -> ( model, Cmd.none )
                Just hub -> ( model, Cmd.batch
                                  [ addHub {lat = hub.hubLat, lng = hub.hubLng}
                                  , Task.perform Failure AddTanks (Tank.getByHub 1)])
        AddTanks tanks ->
            ( model, Cmd.batch (List.map (\t -> addTank (t.tankId, {lat=t.tankLat, lng=t.tankLng}, t.tankYellow, t.tankRed)) tanks))
        AddReadings idx readings ->
            let newCharts = List.map (addReadings idx readings) model.charts
            in ({ model | charts = newCharts}, Cmd.none )
        Failure err ->
            let _ = Debug.log (toString err) err
            in ( model, Cmd.none )
        ChangeScale shours ->
            case String.toInt shours of
                Err _ -> ( model, Cmd.none )
                Ok hours -> ( { model | timeScale = hours, charts = List.map clearChart model.charts }, Cmd.none )
        ToggleManual cid ->
            let charts = List.map (toggleManual cid) model.charts
            in ( { model | charts = charts }, Cmd.none )
        ToggleNoReadings ->
            ( { model | includeNoReadings = not model.includeNoReadings }, Cmd.none)
        RouteRed ->
            let levels = List.map chartToLevel model.charts
                noreadings = if model.includeNoReadings
                             then List.filter (\ (_, l) -> l == "noreadings") levels
                                 |> List.map (\ (i, _) -> i)
                             else []
                manual = List.filter (\c -> c.manual) model.charts |> List.map (\c -> c.id)
                low = List.filter (\ (i, l) -> l == "red" || l == "empty") levels
                    |> List.map (\ (i, _) -> i)
            in ( model, sendRoutes {manual = manual, noreadings = noreadings, low = low})
        RouteYellow ->
            let levels = List.map chartToLevel model.charts
                noreadings = if model.includeNoReadings
                             then List.filter (\ (_, l) -> l == "noreadings") levels
                                 |> List.map (\ (i, _) -> i)
                             else []
                manual = List.filter (\c -> c.manual) model.charts |> List.map (\c -> c.id)
                low = List.filter (\ (i, l) -> l == "yellow" || l == "red" || l == "empty") levels
                    |> List.map (\ (i, _) -> i)
            in ( model, sendRoutes {manual = manual, noreadings = noreadings, low = low})
        FastTick newTime ->
            ( model, Cmd.batch
                    (List.map (\c -> let (i, l) = chartToLevel c in setColor (i, l, c.manual)) model.charts))
        SlowTick newTime ->
            let getReadings = List.map (\c -> Reading.getByTank
                                            c.id
                                            (60 * 60 * model.timeScale |> Just)
                                            c.lastPulled
                            |> Task.perform Failure (AddReadings c.id))
            in ( model, Cmd.batch (getReadings model.charts))

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

svgWidth : Float
svgWidth = 600

svgHeight : Float
svgHeight = 400

maxY : Float
maxY = 4096

xoffset : Float
xoffset = 15

yoffset : Float
yoffset = 0

ystep : Float
ystep = (svgHeight - yoffset) / maxY

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

zip : List a -> List b -> List (a, b)
zip xs ys =
    case (xs, ys) of
        ([], _) -> []
        (_, []) -> []
        (x::xrst, y::yrst) -> (x, y) :: zip xrst yrst

renderVals : Chart -> Html Msg
renderVals chart =
    let vals = chart.values
        yvals = List.map (\v -> v.y) vals
        xvals = List.map (\v -> v.x) vals
        numVals = List.length xvals
        (minYVal, maxYVal) = getRange yvals
        (minXVal, maxXVal) = getRange xvals
        maxX = maxXVal - minXVal + xoffset
        xstep = (svgWidth - xoffset) / maxX
        xSize = maxX * xstep -- (toFloat numVals) * xstep
        left = xoffset - xstep / 4
        right = xSize + xoffset - xstep
        bottom = svgHeight - minYVal * ystep - ystep/2
        top = svgHeight - (maxYVal + 1) * ystep + ystep/2
        leftBar = Svg.line [ S.y1 (toString bottom), S.y2 (toString top)
                           , S.x1 "0", S.x2 "0", S.color "black", S.strokeWidth "5" ] []
        bottomBar = Svg.line [ S.y1 (toString svgHeight), S.y2 (toString svgHeight)
                             , S.x1 (toString left), S.x2 (toString right)
                             , S.color "black", S.strokeWidth "5"] []
        calcX x = xstep * (x - minXVal) + xstep / 2 + xoffset
        calcY y = svgHeight - ystep * y - ystep / 2
        coloring = color (calcY chart.red) (calcY chart.yellow)
        ypts = List.map calcY yvals
        xpts = List.map calcX xvals
        ymarginals = List.map (\y -> Svg.line [ S.x1 "0", S.x2 "5", S.strokeWidth "1"
                                              , S.y1 (toString y), S.y2 (toString y)
                                              ] []) ypts
        xmarginals = List.map (\x -> Svg.line [ S.x1 (toString x), S.x2 (toString x)
                                              , S.strokeWidth "1"
                                              , S.y1 (toString svgHeight), S.y2 (toString (svgHeight - 5))
                                              ] []) xpts
        pts = List.map (\ (x,y) -> Svg.circle [ S.cx (toString x)
                                           , S.cy (toString y)
                                           , S.r "3"
                                           , coloring y |> S.fill
                                           ] [] ) (zip xpts ypts)
        lines = if List.isEmpty ypts then []
                else let ay = Maybe.withDefault -1 (List.head ypts)
                         ax = Maybe.withDefault -1 (List.head xpts)
                    in fst
                        (List.foldl
                             (\ (x', y') (ls, (xacc, yacc)) ->
                                  ((Svg.line [ S.x1 (toString xacc), S.x2 (toString x')
                                             , S.y1 (toString yacc), S.y2 (toString y') ]
                                        []) :: ls
                                  , (x', y'))
                             )
                             ([], (ax, ay))
                             (zip xpts ypts))
        getDate = Maybe.map showDate >> Maybe.withDefault "" >> text
        leftX = Svg.text' [ S.x "0"
                          , S.y "20"
                          , S.textAnchor "start"
                          ]
                [ getDate chart.earliestDate ]
        rightX = Svg.text' [ S.x (toString xSize)
                           , S.y "20"
                           , S.textAnchor "end"
                           ]
                 [ getDate chart.latestDate ]
        marginals = ymarginals ++ xmarginals
        allTogether = leftX :: rightX :: (marginals ++ lines ++ pts)
    in
        Svg.svg [ S.height (toString svgHeight)
                , S.width (toString svgWidth) ]
            (leftBar :: bottomBar :: allTogether)

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

viewChart : Chart -> Html Msg
viewChart chart =
    div [ class "chart-region" ]
     [ div [] [ text ("Chart for Location " ++ chartLocation chart) ]
     , div [] [ button [ onClick (ClearChart chart.id) ] [ text "Clear Chart" ] ]
     , div [ class "chart" ] [ renderVals chart ] ]

getCurrentCharts : Model -> List Chart
getCurrentCharts model =
    case model.currChart of
        Nothing -> []
        Just pos -> List.filter (\c -> c.id == pos) model.charts

view : Model -> Html Msg
view model = div
             [ id "viewing-area" ]
             [ div [ class "buttons" ]
                   [ div [ ] [ text "Include No Readings: "
                             , input [ type' "checkbox", onClick ToggleNoReadings ] [ ]
                             ]
                   , button [ onClick RouteRed ] [ text "Route to Red" ]
                   , button [ onClick RouteYellow ] [ text "Route to Yellow"]
                   ]
             , div [ class "charts" ] (List.map viewChart (getCurrentCharts model)) ,
                   div [] [ text "Hours to View: "
                          , input [ type' "number", placeholder "12", onInput ChangeScale ] []]
             ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ addMarker AddChart
        , markerClicked MarkerClicked
        , markerDblClicked ToggleManual
        -- , addToMarker (\chartval -> AddToChart chartval.id chartval.value)
        -- , updateMarker (\chartval -> UpdateChart chartval.id chartval.value)
        , Time.every (2*second) SlowTick
        , Time.every (500*millisecond) FastTick
        ]

main : Program Never
main =
    Html.App.program
        { init = (initialModel, Task.perform Failure AddHub (Hub.getById 1))
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
