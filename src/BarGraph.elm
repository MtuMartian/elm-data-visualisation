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
import SvgViews exposing (label)


-- MODEL
-- for testing purposes TODO: Replace with more appropriate defaults


defaultModelWithData : List BarDataModel -> String -> Int -> Int -> BarModel
defaultModelWithData data id width height =
    { id = id
    , data = data
    , width = width
    , height = height
    , margin = 60
    , range = ( 0, 30 )
    , min = 0
    , max = 30
    , ticks = 6
    , partLeft = 80
    , partRight = 200
    , partAbove = 40
    , partBelow = 40
    , title = "Bar Chart"
    , vertTitle = "Values"
    , horiTitle = "Entries"
    }


-- VIEW


view : List BarDataModel -> List (Attribute Msg) -> (Int, Int) -> ChartModel -> String -> Html Msg
view data attributes dimensions mdl id =
    let
        model =
            Dict.get id mdl.barGraphs
    in
        case model of
            Just model ->
                div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ]
                    [ svg
                        ( [ height (toString model.height), width (toString model.width), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)) ]
                        ++ attributes
                        )
                        ((axes model) ++ (bars model 0 []) ++ [rect [width (toString model.width), height (toString model.height), fill "none", stroke "black", strokeWidth "2"] []]) -- remove this rect
                    ]

            Nothing ->
                let
                    model =
                        defaultModelWithData data id (Tuple.first dimensions) (Tuple.second dimensions)
                in
                    div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ] []

axes : BarModel -> List (Svg Msg)
axes model =
    List.append
        [ line
            [ x1 (toString model.partLeft)
            , x2 (toString (model.width - model.partRight))
            , y1 (toString (model.height - 10))
            , y2 (toString (model.height - 10))
            , strokeWidth "2"
            , stroke "black"
            ]
            []
        ]
        (crossSections model 0)


crossSections : BarModel -> Int -> List (Svg Msg)
crossSections model iter =
    if iter >= model.ticks then
        []
    else
        let
            total = model.ticks

            height =
                model.height - (model.height * iter // total) - 10
            width =
                toString (model.width - model.partRight)

            range =
              (Tuple.second model.range) - (Tuple.first model.range)

            value =
                (Tuple.first model.range) + ((range / (toFloat total)) * (toFloat iter)) |> toString

        in
            [ line [ x1 (toString model.partLeft), x2 width, y1 ( toString height), y2 (toString height), strokeWidth "1", stroke "gray" ] []
            , Svg.text_
                [ x (toString (model.partLeft - 2)), y (toString (height)), textAnchor "end", alignmentBaseline "middle" ]
                [Svg.text value ]
            ]
            ++ (crossSections model (iter + 1))
--Svg.text_ [ x (toString xCoor), y (toString yCoor), textAnchor "middle" ] [ Svg.text txt ]

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
                    let
                        range = model.range

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
                            model.margin - highlightModifier

                        height =
                            model.height - (truncate ((toFloat model.height) * normalizer)) - margin


                        width =
                            (model.width - 20  - model.partLeft - model.partRight) // (List.length model.data) - margin

                        xCoor =
                            (toFloat (width + margin)) * ((toFloat iter) + 1 / 2) + (toFloat model.partLeft)

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


animateLoad : String -> String -> String -> List (Svg Msg)
animateLoad start end delay =
    [ Svg.animate [ attributeName "y2", from start, to end, dur "1s", begin delay, fill "freeze" ] []
    ]
