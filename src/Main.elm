module Main exposing (..)

import CSS exposing (box)
import Html exposing (Html, div, text, program)
import Html.Attributes exposing (style)
import Model exposing (BarModel, BarDataModel, ChartModel, init, PieModel, PieDataModel, BoxPlotModel, BoxDataModel)
import Msgs exposing (..)
import BarGraph exposing (view, defaultModel, defaultModelWithData)
import ChartingMessages exposing (..)
import BoxPlot exposing (view)
import PieChart exposing (view)
import Update exposing (chartUpdate)


-- model


type alias Model =
    { mdl : ChartModel }



-- init


init : ( Model, Cmd Msg )
init =
    ( { mdl = Model.init }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ BarGraph.view testData model.mdl "1"
        --, BarGraph.view testData2 model.mdl "2"
        , PieChart.view testData3 model.mdl "1"
        , BoxPlot.view testData4 model.mdl "1"
        , textWithBox
        ]


textWithBox : Html Msg
textWithBox =
  div [ style box ]
    [ text "test" ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


testData : List BarDataModel
testData =
    [ { id = 1, value = 1, label = "Longer Label", isHighlighted = False, color = "#005000" }
    , { id = 2, value = 4, label = "p2", isHighlighted = False, color = "#252500" }
    , { id = 3, value = 9, label = "p3", isHighlighted = False, color = "#000050" }
    , { id = 4, value = 16, label = "p4", isHighlighted = False, color = "#202505" }
    , { id = 5, value = 10, label = "p2", isHighlighted = False, color = "#250025" }
    ]


testData2 : List BarDataModel
testData2 =
    [ { id = 1, value = 2, label = "Longer Label", isHighlighted = False, color = "#00ff00" }
    , { id = 2, value = 4, label = "p2", isHighlighted = False, color = "#00ff00" }
    , { id = 3, value = 6, label = "p3", isHighlighted = False, color = "#00ff00" }
    , { id = 4, value = 8, label = "p4", isHighlighted = False, color = "#00ff00" }
    , { id = 5, value = 10, label = "p2", isHighlighted = False, color = "#00ff00" }
    , { id = 6, value = 8, label = "p4", isHighlighted = False, color = "#00ff00" }
    , { id = 7, value = 10, label = "p2", isHighlighted = False, color = "#00ff00" }
    ]

testData3 : List PieDataModel
testData3 =
    [ { id = 1, value = 2, label = "Longer Label" }
    , { id = 2, value = 4, label = "p2" }
    , { id = 3, value = 6, label = "p3" }
    , { id = 4, value = 8, label = "p4" }
    ]

testData4 : List BoxDataModel
testData4 =
    [ { id = 1, value = 2 }
    , { id = 2, value = 4 }
    , { id = 3, value = 6 }
    , { id = 4, value = 8 }
    ]



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg_ msg_ ->
            let
                ( updatedModel, cmd ) =
                    chartUpdate msg_ model.mdl
            in
                ( { model | mdl = updatedModel }, cmd )
