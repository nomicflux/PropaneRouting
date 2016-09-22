module SharedModels exposing (..)

type alias ChartID = Int
type alias GMPos = { lat : Float, lng: Float }
type alias ChartPt = {x: Float, y: Float}
type alias ChartVal = { id : ChartID, value: ChartPt }
type alias Level = String
type alias RoutedTanks = { manual: List ChartID
                         , noreadings: List ChartID
                         , low: List ChartID
                         }
