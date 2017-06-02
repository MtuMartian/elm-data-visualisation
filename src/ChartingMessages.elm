module ChartingMessages exposing (..)

import Model exposing (..)


type ChartMsg
    = BarGraphMouseOver BarDataModel String
    | BarGraphMouseOut BarDataModel String
    | PieChartMouseOver PieDataModel String
    | PieChartMouseOut PieDataModel String
    | BubbleChartMouseOver BubbleDataModel String
    | BubbleChartMouseOut BubbleDataModel String
    | BarGraphCreated String BarModel
    | PieChartCreated String PieModel
    | BoxPlotCreated String BoxPlotModel
    | BubbleChartCreated String BubbleChartModel
