port module GMPorts exposing (..)

import SharedModels exposing (..)

-- Incoming

port addMarker : ((ChartID, GMPos, Float, Float) -> msg) -> Sub msg

port markerClicked : (ChartID -> msg) -> Sub msg

port markerDblClicked : (ChartID -> msg) -> Sub msg

-- Outgoing

port addHub : GMPos -> Cmd msg

port addTank : (ChartID, GMPos, Float, Float) -> Cmd msg

port setColor : (ChartID, Level, Bool) -> Cmd msg

port sendRoutes : RoutedTanks -> Cmd msg

port clearTanks : Bool -> Cmd msg
