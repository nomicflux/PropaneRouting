module Propane exposing (..)

import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
-- import String
-- import Result exposing (Result)
import Svg
import Svg.Attributes as S
import SharedModels exposing (..)
import GMPorts exposing (..)
import Array as A
import Task
import Http
import Debug
import Time exposing (Time, second)
import HubAPI as Hub
import TankAPI as Tank
import ReadingAPI as Reading

type alias Chart = { id : ChartID, pos : GMPos, values : A.Array ChartPt, yellow : Float, red : Float }
type alias Model = { charts : List Chart, numCharts : Int, currChart : Maybe ChartID }

type Msg = ClearChart ChartID
         | AddChart (ChartID, GMPos, Float, Float)
         | MarkerClicked ChartID
         | AddHub (Maybe Hub.HubRead)
         | AddTanks (List Tank.TankRead)
         | AddReadings ChartID (List Reading.ReadingRead)
         | Failure Http.Error
         | Tick Time

getLast : A.Array number -> number
getLast arr = Maybe.withDefault 0 (A.get (A.length arr - 1) arr)

initialModel : Model
initialModel = { charts = [ ], numCharts = 0, currChart = Nothing }

addReadings : ChartID -> List Reading.ReadingRead -> Chart -> Chart
addReadings id vals chart =
    if chart.id /= id
        then chart
        else { chart | values = A.fromList (List.map (\v -> {x = v.readingSensorSent, y = v.readingValue}) vals) }

clearChart : Chart -> Chart
clearChart chart =
    let lastVal = A.get (A.length chart.values - 1) chart.values
    in { chart | values = A.fromList [] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClearChart idx ->
            let newCharts = List.map (\c -> if c.id == idx then clearChart c else c) model.charts
            in ({ model | charts = newCharts}, Cmd.none )
        AddChart (id, pos, yellow, red) ->
            let newChart = { id = id, pos = pos, values = A.fromList [], yellow = yellow, red = red }
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
        Tick newTime ->
            let readings = Debug.log "Readings"

            in ( model, Cmd.batch (List.map (\c -> Reading.getByTank c.id |> Task.perform Failure (AddReadings c.id)) model.charts))

svgWidth : Float
svgWidth = 800

svgHeight : Float
svgHeight = 400

maxY : Float
maxY = 4096

-- maxX : Float
-- maxX = 1024

xoffset : Float
xoffset = 15

yoffset : Float
yoffset = 0

-- xstep : Float
-- xstep = (svgWidth - xoffset) / maxX

ystep : Float
ystep = (svgHeight - yoffset) / maxY

getRange : A.Array Float -> (Float, Float)
getRange arr =
    let (minval, maxval) = A.foldl (\x (cmin, cmax) ->
                                        let nmin = Basics.min cmin x
                                            nmax = Basics.max cmax x
                                        in (nmin, nmax)
                                   ) (1/0, -1/0) arr
        retMin = if isInfinite minval then 0 else minval
        retMax = if isInfinite maxval then 0 else maxval
    in (retMin, retMax)

arrzip: A.Array a -> A.Array b -> List (a, b)
arrzip arr1 arr2 =
    List.map2 (,) (A.toList arr1) (A.toList arr2)

color : Float -> Float -> Float -> String
color red yellow val =
    if val <= yellow then "green"
    else if val <= red then "yellow"
         else "red"

renderVals : Chart -> Html Msg
renderVals chart =
    let vals = chart.values
        yvals = A.map (\v -> v.y) vals
        xvals = A.map (\v -> v.x) vals
        numVals = A.length xvals
        (minYVal, maxYVal) = getRange yvals
        (minXVal, maxXVal) = getRange xvals
        maxX = maxXVal - minXVal + xoffset
        xstep = (svgWidth - xoffset) / maxX
        xSize = (maxXVal - minXVal) * xstep -- (toFloat numVals) * xstep
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
        -- _ = Debug.log "XMax" maxXVal * xstep
        -- _ = Debug.log "XMax1" (maxXVal - minXVal)*xstep
        -- _ = Debug.log "XMax2" (maxX * xstep)
        calcY y = svgHeight - ystep * y - ystep / 2
        coloring = color (calcY chart.red) (calcY chart.yellow)
        ypts = A.map calcY yvals
        xpts = A.map calcX xvals
        -- xpts = A.initialize numVals (toFloat >> (*) xstep >> (+) xoffset)
        ymarginals = A.map (\y -> Svg.line [ S.x1 "0", S.x2 "5", S.strokeWidth "1"
                                           , S.y1 (toString y), S.y2 (toString y)
                                           ] []) ypts
        xmarginals = A.map (\x -> Svg.line [ S.x1 (toString x), S.x2 (toString x)
                                           , S.strokeWidth "1"
                                           , S.y1 (toString svgHeight), S.y2 (toString (svgHeight - 5))
                                           ] []) xpts
        pts = List.map (\ (x,y) -> Svg.circle [ S.cx (toString x)
                                           , S.cy (toString y)
                                           , S.r "3"
                                           , coloring y |> S.fill
                                           ] [] ) (arrzip xpts ypts)
        lines = if A.isEmpty ypts then []
                else let ay = Maybe.withDefault -1 (A.get 0 ypts)
                         ax = Maybe.withDefault -1 (A.get 0 xpts)
                    in fst
                        (List.foldl
                             (\ (x', y') (ls, (xacc, yacc)) ->
                                  ((Svg.line [ S.x1 (toString xacc), S.x2 (toString x')
                                             , S.y1 (toString yacc), S.y2 (toString y') ]
                                        []) :: ls
                                  , (x', y'))
                             )
                             ([], (ax, ay))
                             (arrzip xpts ypts))
        marginals = A.toList (A.append ymarginals xmarginals)
        allTogether = marginals ++ pts ++ lines
    in
        Svg.svg [ S.height (toString svgHeight)
                , S.width (toString svgWidth) ]
            (leftBar :: bottomBar :: allTogether)

chartLocation : Chart -> String
chartLocation chart = toString chart.id

viewChart : Chart -> Html Msg
viewChart chart =
    div [ class "chart-region" ]
     [ div [] [ text ("Chart for Location " ++ chartLocation chart) ]
     , div [] [ button [ onClick (ClearChart chart.id) ] [ text "Clear Chart" ] ]
     -- , div [] [ button [ onClick (UpdateChart chart.id 10.0) ] [ text "Refill Tank" ] ]
     -- , div [] [ button [ onClick (UpdateChart chart.id -20.0) ] [ text "Empty Tank" ] ]
     , div [ class "chart" ] [ renderVals chart ] ]

getCurrentCharts : Model -> List Chart
getCurrentCharts model =
    case model.currChart of
        Nothing -> []
        Just pos -> List.filter (\c -> c.id == pos) model.charts

view : Model -> Html Msg
view model = div
             [ id "viewing-area" ]
             [ div [ class "charts" ] (List.map viewChart (getCurrentCharts model))
             ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ addMarker AddChart
        , markerClicked MarkerClicked
        -- , addToMarker (\chartval -> AddToChart chartval.id chartval.value)
        -- , updateMarker (\chartval -> UpdateChart chartval.id chartval.value)
        , Time.every second Tick
        ]

main : Program Never
main =
    Html.App.program
        { init = (initialModel, Task.perform Failure AddHub (Hub.getById 1))
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
