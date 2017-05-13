module BarGraph exposing (..)

import Array
import Attributes exposing (..)
import Dict exposing (..)
import Html exposing (Html, div, text)
import Model exposing (..)
import Msgs exposing (Msg)
import ChartingMessages exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events exposing (onClick, onMouseOver, onMouseOut)
import Json.Decode as Json
import Html.Events as HEvents exposing (on)


-- MODEL
-- for testing purposes TODO: Replace with more appropriate defaults


defaultModel : BarModel
defaultModel =
    { id = "1"
    , data = defaultData
    , height = 600
    , width = 800
    , range = Just ( 0, 10 )
    }


defaultModelWithData : List BarDataModel -> String -> BarModel
defaultModelWithData data id =
    { id = id
    , data = data
    , height = 600
    , width = 800
    , range = Just ( 0, 30 )
    }


defaultData : List BarDataModel
defaultData =
    [ { id = 1, value = 1, label = "Longer Label", isHighlighted = False }
    , { id = 2, value = 2, label = "p2", isHighlighted = False }
    , { id = 3, value = 3, label = "p3", isHighlighted = False }
    , { id = 4, value = 5, label = "p4", isHighlighted = False }
    , { id = 5, value = 6, label = "p2", isHighlighted = False }
    ]



-- UPDATE
-- VIEW


view : List BarDataModel -> ChartModel -> String -> Html Msg
view data mdl id =
    let
        model =
            Dict.get id mdl.barGraphs

        --model = defaultModelWithData data
    in
        case model of
            Just model ->
                div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ]
                    [ svg
                        [ width (toString model.width), height (toString model.height), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)) ]
                        ((axes model) ++ (bars model 0))
                    ]

            Nothing ->
                let
                    model =
                        defaultModelWithData data id
                in
                    div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ] []


axes : BarModel -> List (Svg Msg)
axes model =
    List.append
        [ line
            [ x1 "10"
            , x2 "10"
            , y1 "0"
            , y2 (toString (model.height - 10))
            , strokeWidth "2"
            , stroke "black"
            ]
            []
        , line
            [ x1 "10"
            , x2 (toString (model.width - 10))
            , y1 (toString (model.height - 10))
            , y2 (toString (model.height - 10))
            , strokeWidth "2"
            , stroke "black"
            ]
            []
        ]
        (ticks model 0 6)


ticks : BarModel -> Int -> Int -> List (Svg Msg)
ticks model iter total =
    if iter >= total then
        []
    else
        let
            height =
                toString (model.height * iter // total)
        in
            (line [ x1 "0", x2 "20", y1 height, y2 height, strokeWidth "2", stroke "black" ] []) :: (ticks model (iter + 1) total)


bars : BarModel -> Int -> List (Svg Msg)
bars model iter =
    if iter >= (List.length model.data) then
        []
    else
        let
            bar =
                Array.get iter (Array.fromList (model.data))
        in
            case bar of
                Just bar ->
                    case model.range of
                        Just range ->
                            let
                                highlightModifier =
                                    if bar.isHighlighted then
                                        5
                                    else
                                        0

                                min =
                                    Tuple.first range

                                max =
                                    Tuple.second range

                                normalizer =
                                    (bar.value - min) / (max - min)

                                height =
                                    model.height - (truncate ((toFloat model.height) * normalizer))

                                margin =
                                    10 - highlightModifier

                                width =
                                    (model.width - 20) // (List.length model.data) - margin

                                xCoor =
                                    (toFloat (width + margin)) * ((toFloat iter) + 1 / 2) + 20

                                delay =
                                    (toString ((toFloat iter) / 24)) ++ "s"

                                start =
                                    (toString (model.height - 10))

                                end =
                                    (toString height)

                                test =
                                    Debug.log "Testing bar: " bar

                                labelString =
                                    if bar.isHighlighted then
                                        bar.label
                                    else
                                        ""
                            in
                                ([ line
                                    [ y1 start
                                    , y2 start
                                    , x1 (toString xCoor)
                                    , x2 (toString xCoor)
                                    , strokeWidth (toString width)
                                    , stroke "black"
                                    , onMouseOver (Msgs.Msg_ (BarGraphMouseOver bar model.id))
                                    , onMouseOut (Msgs.Msg_ (BarGraphMouseOut bar model.id))
                                    ]
                                    (animateLoad start end delay)
                                 , (label xCoor (toFloat (height - 10)) labelString)
                                 ]
                                )
                                    ++ (bars model (iter + 1))

                        Nothing ->
                            []

                Nothing ->
                    []


label : Float -> Float -> String -> Svg Msg
label xCoor yCoor txt =
    Svg.text_ [ x (toString xCoor), y (toString yCoor), textAnchor "middle" ] [ Svg.text txt ]


animateLoad : String -> String -> String -> List (Svg Msg)
animateLoad start end delay =
    [ Svg.animate [ attributeName "y2", from start, to end, dur "1s", begin delay, fill "freeze" ] []
    ]
