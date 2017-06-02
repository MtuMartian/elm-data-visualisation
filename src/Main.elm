module Main exposing (..)

import CSS exposing (box)
import Html exposing (Html, div, text, program)
import Html.Attributes exposing (style)
import Model exposing (..)
import Msgs exposing (..)
import BarGraph exposing (view, defaultModelWithData)
import ChartingMessages exposing (..)
import BoxPlot exposing (view)
import PieChart exposing (view)
import BubbleChart exposing (view)
import Update exposing (chartUpdate)

--import Svg exposing (..)
import Svg.Attributes as Attributes exposing (opacity)

import Properties as Props exposing (..)



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
        [ BarGraph.view testData2 [ Props.title "Different title", Props.margin "8", horiAxisTitle "STUFFS", Props.max "15", Props.min "0" ] (1000, 600) model.mdl "1"
        , BarGraph.view testData [ Props.title "Second Data Set", Props.margin "16"] (1000, 300) model.mdl "2"
        , PieChart.view testData3 model.mdl "1"
        , BubbleChart.view testData4 [ Props.margin "5", Props.partitionLeft "240" ] (1200, 600) model.mdl "1"
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
    [ { id = 1, value = 7, label = "Longer Label", isHighlighted = False, color = "#005000" }
    , { id = 2, value = 24, label = "p2", isHighlighted = False, color = "#252500" }
    , { id = 3, value = 9, label = "p3", isHighlighted = False, color = "#000050" }
    , { id = 4, value = 20, label = "p4", isHighlighted = False, color = "#202505" }
    , { id = 5, value = 10, label = "p2", isHighlighted = False, color = "#250025" }
    ]


testData2 : List BarDataModel
testData2 =
    [ { id = 1, value = 8, label = "Longer Label", isHighlighted = False, color = "#005000" }
    , { id = 2, value = 4, label = "p2", isHighlighted = False, color = "#000f30" }
    , { id = 3, value = 6, label = "p3", isHighlighted = False, color = "#4581f2" }
    , { id = 4, value = 3, label = "p4", isHighlighted = False, color = "#a2b3c4" }
    , { id = 5, value = 10, label = "p2", isHighlighted = False, color = "#004488" }
    , { id = 6, value = 8, label = "p4", isHighlighted = False, color = "#050278" }
    , { id = 7, value = 10, label = "p2", isHighlighted = False, color = "#5fff5f" }
    ]

testData3 : List PieDataModel
testData3 =
    [ { id = 1, value = 2, label = "Longer Label", isHighlighted = False }
    , { id = 2, value = 4, label = "p2", isHighlighted = False }
    , { id = 3, value = 6, label = "p3", isHighlighted = False }
    , { id = 4, value = 8, label = "p4", isHighlighted = False }
    ]

testData4 : List BubbleDataModel
testData4 =
    [ { id = 1, value = 2, valueHori = 1, valueVert = 4, label = "d1", isHighlighted = False}
    , { id = 2, value = 4, valueHori = 1, valueVert = 10, label = "d2", isHighlighted = False}
    , { id = 3, value = 6, valueHori = 5, valueVert = 7, label = "d3", isHighlighted = False}
    , { id = 4, value = 8, valueHori = 2, valueVert = 6, label = "d4", isHighlighted = False}
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
