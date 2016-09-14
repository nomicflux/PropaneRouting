port module GMPorts exposing (..)

import SharedModels exposing (..)

port addMarker : (GMPos -> msg) -> Sub msg

port markerClicked : (GMPos -> msg) -> Sub msg

port addToMarker : (GMPosWVal -> msg) -> Sub msg

port updateMarker : (GMPosWVal -> msg) -> Sub msg

port sendChartVal : List GMPosWVal -> Cmd msg
