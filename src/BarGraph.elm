module BarGraph exposing (..)

import Array
import Attributes exposing (..)
import CSS exposing (box)
import Dict exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes as HAttr exposing (style)
import Model exposing (..)
import Msgs exposing (Msg)
import ChartingMessages exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events exposing (onClick, onMouseOver, onMouseOut)


-- MODEL
-- for testing purposes TODO: Replace with more appropriate defaults


defaultModel : BarModel
defaultModel =
    { id = "1"
    , data = []
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


-- VIEW


view : List BarDataModel -> ChartModel -> String -> Html Msg
view data mdl id =
    let
        model =
            Dict.get id mdl.barGraphs
    in
        case model of
            Just model ->
                div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ]
                    [ svg
                        [ width (toString model.width), height (toString model.height), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)) ]
                        ((axes model) ++ (bars model 0 []))-- ++ boxText
                    ]

            Nothing ->
                let
                    model =
                        defaultModelWithData data id
                in
                    div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ] []

boxText : Svg Msg
boxText =
  foreignObject [x "20", y "20"] [div [HAttr.style box] [Html.text "hello world"]]

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


bars : BarModel -> Int -> List (Svg Msg) -> List (Svg Msg)
bars model iter infoBoxes =
    if iter >= (List.length model.data) then
        infoBoxes
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

                                margin =
                                    10 - highlightModifier

                                height =
                                    model.height - (truncate ((toFloat model.height) * normalizer)) - margin


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

                                infoBox = label xCoor (toFloat (height - 50)) bar.label (toString bar.value)

                                updatedInfoBoxes = if bar.isHighlighted then
                                  infoBoxes ++ [infoBox]
                                else
                                  infoBoxes

                            in
                                ([ line
                                    [ y1 start
                                    , y2 start
                                    , x1 (toString xCoor)
                                    , x2 (toString xCoor)
                                    , strokeWidth (toString width)
                                    , stroke bar.color
                                    , onMouseOver (Msgs.Msg_ (BarGraphMouseOver bar model.id))
                                    , onMouseOut (Msgs.Msg_ (BarGraphMouseOut bar model.id))
                                    ]
                                    (animateLoad start end delay)
                                 ]
                                )
                                    ++ (bars model (iter + 1) updatedInfoBoxes)

                        Nothing ->
                            []

                Nothing ->
                    []


label : Float -> Float -> String -> String -> Svg Msg
label xCoor yCoor label value =
  foreignObject [ x (toString xCoor), y (toString yCoor) ] [ div [HAttr.style box] [Html.b [] [Html.text (label ++ ": ")], Html.text value]]
    --Svg.text_ [ x (toString xCoor), y (toString yCoor), textAnchor "middle" ] [ Svg.text txt ]


animateLoad : String -> String -> String -> List (Svg Msg)
animateLoad start end delay =
    [ Svg.animate [ attributeName "y2", from start, to end, dur "1s", begin delay, fill "freeze" ] []
    ]
