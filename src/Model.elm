module Model exposing (..)

import Dict exposing (..)


-- MODELS


type alias ChartModel =
    { barGraphs : Dict String BarModel }


init : ChartModel
init =
    { barGraphs = Dict.empty }


type alias Model =
    { chartType : ChartType
    , data : List DataModel
    , label : String
    , height : Int
    , width : Int
    , range : Maybe ( Float, Float )
    }


type ChartType
    = BarGraph
    | PieChart
    | BoxPlot


type alias DataModel =
    { id : Int
    , value : Float
    , label : String
    , isHighlighted : Bool
    }


type alias BarModel =
    { id : String
    , data : List DataModel
    , height : Int
    , width : Int
    , range : Maybe ( Float, Float )
    }


type alias BarDataModel =
    { id : Int
    , value : Float
    , label : String
    , isHighlighted : Bool
    }
