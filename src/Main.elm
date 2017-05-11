module Main exposing (..)

import Html exposing (Html, div, text, program)
import Model exposing (..)
import Msgs exposing (..)
import Update exposing (update)
import BarGraph exposing (view)
import BoxPlot exposing (view)
import PieChart exposing (view)


-- init


init : ( Model.Model, Cmd Msg )
init =
    ( testModel, Cmd.none )



-- VIEW
{--view : BarGraph.Model -> Html Msg
view model =
    div []
        [(BarGraph.view model)]
--}


view : Model.Model -> Html Msg
view model =
    case model.chartType of
        BarGraph ->
            div []
                [ (BarGraph.view model) ]

        PieChart ->
            div []
                [ (PieChart.view model) ]

        BoxPlot ->
            div []
                [ (BoxPlot.view model) ]



-- SUBSCRIPTIONS


subscriptions : Model.Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model.Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


testModel : Model.Model
testModel =
    { chartType = BarGraph
    , data = testData
    , label = "test"
    , height = 1200
    , width = 400
    , range = Just ( 0, 10 )
    }


testData : List DataModel
testData =
    [ { id = 1, value = 1, label = "Longer Label", isHighlighted = False }
    , { id = 2, value = 2, label = "p2", isHighlighted = False }
    , { id = 3, value = 3, label = "p3", isHighlighted = False }
    , { id = 4, value = 5, label = "p4", isHighlighted = False }
    , { id = 5, value = 6, label = "p2", isHighlighted = False }
    , { id = 6, value = 1, label = "Longer Label", isHighlighted = False }
    , { id = 7, value = 2, label = "p2", isHighlighted = False }
    , { id = 8, value = 8, label = "p3", isHighlighted = False }
    , { id = 9, value = 5, label = "p4", isHighlighted = False }
    , { id = 10, value = 6, label = "p2", isHighlighted = False }
    , { id = 11, value = 1, label = "Longer Label", isHighlighted = False }
    , { id = 12, value = 2, label = "p2", isHighlighted = False }
    , { id = 13, value = 6, label = "p3", isHighlighted = False }
    , { id = 14, value = 7, label = "p4", isHighlighted = False }
    , { id = 15, value = 0.1, label = "p2", isHighlighted = False }

    --}
    {--, { id = 10, value = 7.0, label = "p4", isHighlighted = False }
  , { id = 11, value = 3.5, label = "p2", isHighlighted = False }
  , { id = 12, value = 2.5, label = "p3", isHighlighted = False }
  , { id = 13, value = 7.0, label = "p4", isHighlighted = False } --}
    ]
