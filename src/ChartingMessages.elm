module ChartingMessages exposing (..)

import Model exposing (..)


type ChartMsg
    = BarGraphMouseOver BarDataModel String
    | BarGraphMouseOut BarDataModel String
    | BarGraphCreated String BarModel
