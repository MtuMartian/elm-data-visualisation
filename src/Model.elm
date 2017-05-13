module Model exposing (..)

import Dict exposing (..)


-- MODELS


type alias ChartModel =
    { barGraphs : Dict String BarModel
    , pieCharts : Dict String PieModel
    , boxPlots : Dict String BoxPlotModel
    }


init : ChartModel
init =
    { barGraphs = Dict.empty
    , pieCharts = Dict.empty
    , boxPlots = Dict.empty
    }


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


type alias PieModel =
  { id : String
  , data : List PieDataModel
  , radius : Int
  }

type alias PieDataModel =
  { id : Int
  , value : Float
  , label : String
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

type alias BoxPlotModel =
  { id : String
  , data : List BoxDataModel
  , height : Int
  , width : Int
  }

type alias BoxDataModel =
  { id : Int
  , value : Float
  }
