module Model exposing (..)

-- MODELS

type alias Model =
  { chartType : ChartType
  , data : List DataModel
  , label : String
  , height : Int
  , width : Int
  , range : Maybe (Float, Float)
  }


type ChartType
  = BarGraph
  | PieChart

type alias DataModel =
  { id : Int
  , value : Float
  , label : String
  , isHighlighted : Bool
  }
