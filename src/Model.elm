module Model exposing (..)

import Dict exposing (..)


-- MODELS

init : ChartModel
init =
    { barGraphs = Dict.empty
    , pieCharts = Dict.empty
    , boxPlots = Dict.empty
    , bubbleCharts = Dict.empty
    }

type alias ChartModel =
    { barGraphs : Dict String BarModel
    , pieCharts : Dict String PieModel
    , boxPlots : Dict String BoxPlotModel
    , bubbleCharts : Dict String BubbleChartModel
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
  , isHighlighted : Bool
  }

type alias BarModel =
    { id : String
    , data : List BarDataModel
    , width : Int
    , height : Int
    , margin : Int
    , range : ( Float, Float )
    , min : Float
    , max : Float
    , ticks : Int
    , partLeft : Int
    , partRight : Int
    , partAbove : Int
    , partBelow : Int
    , title : String
    , vertTitle : String
    , horiTitle : String
    }

type alias BarDataModel =
    { id : Int
    , value : Float
    , label : String
    , isHighlighted : Bool
    , color : String
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

type alias BubbleChartModel =
  { id : String
  , data : List BubbleDataModel
  , width : Int
  , height : Int
  , rangeHori : (Float, Float)
  , rangeVert : (Float, Float)
  , margin : Int
  , bubbleSize : Float
  , ticksVertical : Int
  , ticksHorizontal : Int
  }

type alias BubbleDataModel =
  { id : Int
  , value : Float
  , valueHori : Float
  , valueVert : Float
  , label : String
  , isHighlighted : Bool
  }
