port module GMPorts exposing (..)

import SharedModels exposing (..)

-- Incoming

port addMarker : ((ChartID, GMPos, Float, Float) -> msg) -> Sub msg

port markerClicked : (ChartID -> msg) -> Sub msg

-- port addToMarker : (ChartVal -> msg) -> Sub msg

-- port updateMarker : (ChartVal -> msg) -> Sub msg

-- Outgoing

port addHub : GMPos -> Cmd msg

port addTank : (ChartID, GMPos, Float, Float) -> Cmd msg

port setColor : (ChartID, Level) -> Cmd msg

-- port sendChartVal : List ChartVal -> Cmd msg
