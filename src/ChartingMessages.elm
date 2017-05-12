module ChartingMessages exposing (..)

import Model exposing (..)

type ChartMsg
  = BarGraphMouseOver BarDataModel
  | BarGraphMouseOut BarDataModel
  | ElementCreated ChartType String
