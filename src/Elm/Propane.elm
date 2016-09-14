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

type alias Chart = { id : GMPos, values : A.Array Float }
type alias Model = { charts : List Chart, numCharts : Int, currChart : Maybe GMPos }

type Msg = AddToChart GMPos Float
         | UpdateChart GMPos Float
         | ClearChart GMPos
         | AddChart GMPos
         | MarkerClicked GMPos

getLast : A.Array number -> number
getLast arr = Maybe.withDefault 0 (A.get (A.length arr - 1) arr)

startingVals : A.Array Float
startingVals = A.fromList [10]

blankChart : Chart
blankChart = { id = {lat = 0, lng = 0}
             , values = startingVals
             }

initialModel : Model
initialModel = { charts = [ ], numCharts = 0, currChart = Nothing }

addToChart : Chart -> Float -> Chart
addToChart chart val =
    { chart | values = A.push val chart.values }

updateChart : Chart -> Float -> Chart
updateChart chart val =
    let recentVal = getLast chart.values
        newVal = Basics.max (recentVal + val) 0
    in { chart | values = A.push newVal chart.values}

clearChart : Chart -> Chart
clearChart chart =
    let lastVal = A.get (A.length chart.values - 1) chart.values
    in { chart | values = A.fromList [Maybe.withDefault 10 lastVal] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddToChart idx val ->
            let newCharts = List.map (\c -> if c.id == idx then addToChart c val else c) model.charts
            in ({ model | charts = newCharts}, Cmd.none )
        UpdateChart idx val ->
            let newCharts = List.map (\c -> if c.id == idx then updateChart c val else c) model.charts
                latestVals = List.filter (\x -> x.value < 5) (List.map (\c -> {pos = c.id, value = Maybe.withDefault 0 (A.get (A.length c.values - 1) c.values)} ) model.charts)
            in ({ model | charts = newCharts}, sendChartVal latestVals )
        ClearChart idx ->
            let newCharts = List.map (\c -> if c.id == idx then clearChart c else c) model.charts
            in ({ model | charts = newCharts}, Cmd.none )
        AddChart pos ->
            let newChart = { blankChart | id = pos }
                newCt = model.numCharts + 1
            in ( { model | charts = newChart :: model.charts, numCharts = newCt }, Cmd.none)
        MarkerClicked pos ->
            ( { model | currChart = Just pos }, Cmd.none )

svgWidth : Float
svgWidth = 800

svgHeight : Float
svgHeight = 200

xstep : Float
xstep = 5

xoffset : Float
xoffset = 20

ystep : Float
ystep = 10

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

renderVals : A.Array Float -> Html Msg
renderVals vals =
    let (minVal, maxVal) = getRange vals
        numVals = A.length vals
        xSize = (toFloat numVals) * xstep
        left = xoffset - xstep / 4
        right = xSize + xoffset - xstep
        bottom = svgHeight - minVal * ystep - ystep/2
        top = svgHeight - (maxVal + 1) * ystep + ystep/2
        leftBar = Svg.line [ S.y1 (toString bottom), S.y2 (toString top)
                           , S.x1 "0", S.x2 "0", S.color "black", S.strokeWidth "5" ] []
        bottomBar = Svg.line [ S.y1 (toString svgHeight), S.y2 (toString svgHeight)
                             , S.x1 (toString left), S.x2 (toString right)
                             , S.color "black", S.strokeWidth "5"] []
        calcY y = svgHeight - ystep * y - ystep / 2
        ypts = A.map calcY vals
        xpts = A.initialize numVals (toFloat >> (*) xstep >> (+) xoffset)
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
                                           , S.fill "red"
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
chartLocation chart = toString chart.id.lat ++ ", " ++ toString chart.id.lng

viewChart : Chart -> Html Msg
viewChart chart =
    div [ class "chart-region" ]
     [ div [] [ text ("Chart for Location " ++ chartLocation chart) ]
     , div [] [ button [ onClick (ClearChart chart.id) ] [ text "Clear Chart" ] ]
     , div [] [ button [ onClick (UpdateChart chart.id 10.0) ] [ text "Refill Tank" ] ]
     , div [] [ button [ onClick (UpdateChart chart.id -20.0) ] [ text "Empty Tank" ] ]
     , div [ class "chart" ] [ renderVals chart.values ] ]

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
        , addToMarker (\poswval -> AddToChart poswval.pos poswval.value)
        , updateMarker (\poswval -> UpdateChart poswval.pos poswval.value)
        ]

main : Program Never
main =
    Html.App.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
