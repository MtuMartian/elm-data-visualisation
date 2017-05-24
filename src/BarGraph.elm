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
import Properties exposing (..)

import Util exposing (tupleToDict)


-- MODEL
-- for testing purposes TODO: Replace with more appropriate defaults


defaultModelWithData : List BarDataModel -> Dict String String -> String -> Int -> Int -> BarModel
defaultModelWithData data properties id width height =
    { id = id
    , data = data
    , width = width
    , height = height
    , margin = Dict.get "margin" properties |> Maybe.withDefault "40" |> String.toInt |> Result.withDefault 40
    , range = ( 0, 15 )
    , min = Dict.get "min" properties |> Maybe.withDefault "0" |> String.toFloat |> Result.withDefault 0
    , max = Dict.get "max" properties |> Maybe.withDefault "30" |> String.toFloat |> Result.withDefault 30
    , ticks = Dict.get "ticks" properties |> Maybe.withDefault "6" |> String.toInt |> Result.withDefault 6
    , partLeft = Dict.get "partitionLeft" properties |> Maybe.withDefault "120" |> String.toInt |> Result.withDefault 120
    , partRight = Dict.get "partitionRight" properties |> Maybe.withDefault "120" |> String.toInt |> Result.withDefault 120
    , partAbove = Dict.get "partitionAbove" properties |> Maybe.withDefault "50" |> String.toInt |> Result.withDefault 50
    , partBelow = Dict.get "partitionBelow" properties |> Maybe.withDefault "50" |> String.toInt |> Result.withDefault 50
    , title = Dict.get "title" properties |> Maybe.withDefault ""
    , vertTitle = Dict.get "vertTitle" properties |> Maybe.withDefault ""
    , horiTitle = Dict.get "horiTitle" properties |> Maybe.withDefault ""
    }


-- VIEW


view : List BarDataModel -> List Property -> (Int, Int) -> ChartModel -> String -> Html Msg
view data properties dimensions mdl id =
    let
        model =
            Dict.get id mdl.barGraphs
    in
        case model of
            Just model ->
                div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ]
                    [ svg
                        [ height (toString model.height), width (toString model.width), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)) ]
                        ((axes model) ++ (bars model 0 []) ++ [rect [width (toString model.width), height (toString model.height), fill "none", stroke "black", strokeWidth "2"] []]) -- remove this rect
                    ]

            Nothing ->
                let
                    propertyDict = tupleToDict properties
                    model =
                        defaultModelWithData data propertyDict id (Tuple.first dimensions) (Tuple.second dimensions)
                in
                    div [ onCreate (Msgs.Msg_ (BarGraphCreated id model)) ] []

axes : BarModel -> List (Svg Msg)
axes model =
    --List.append
        [ line
            [ x1 (toString model.partLeft)
            , x2 (toString (model.width - model.partRight))
            , y1 (toString (model.height - model.partBelow))
            , y2 (toString (model.height - model.partBelow))
            , strokeWidth "2"
            , stroke "black"
            ]
            []
        ]
        ++
        (crossSections model 0)
        ++
        (horiAxisTitle model)
        ++
        (vertAxisTitle model)
        ++
        ( mainTitle model)

horiAxisTitle : BarModel -> List (Svg Msg)
horiAxisTitle model =
  let
    contentWidth = model.width - model.partLeft - model.partRight
    xCoor = contentWidth // 2 + model.partLeft |> toString
    yCoor = model.height - model.partBelow // 2 |> toString

  in
    [ Svg.text_
        [ x xCoor, y yCoor, textAnchor "middle", alignmentBaseline "middle" ]
        [ Svg.text model.horiTitle ]
    ]

vertAxisTitle : BarModel -> List (Svg Msg)
vertAxisTitle model =
  let
    contentHeight = model.height - model.partBelow - model.partAbove
    xCoor = model.partLeft // 2 |> toString
    yCoor = contentHeight // 2 + model.partAbove |> toString

  in
    [ Svg.text_
        [ x xCoor, y yCoor, width (toString model.partLeft), textAnchor "middle", alignmentBaseline "middle" ]
        [ Svg.text model.vertTitle ]
    ]

mainTitle : BarModel -> List (Svg Msg)
mainTitle model =
  let
    xCoor = model.width // 2 |> toString
    yCoor = model.partAbove // 2 |> toString

  in
    [ Svg.text_
        [ x xCoor, y yCoor, textAnchor "middle", alignmentBaseline "middle" ]
        [ Svg.text model.title ]
    ]




crossSections : BarModel -> Int -> List (Svg Msg)
crossSections model iter =
    if iter >= model.ticks then
        []
    else
        let
            total = model.ticks

            height =
                model.height - ((model.height - model.partAbove) * iter // total) - model.partBelow
            width =
                toString (model.width - model.partRight)

            range =
              model.max - model.min

            value =
                model.min + ((range / (toFloat total)) * (toFloat iter)) |> toString

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
                        range = (model.min, model.max)

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
                            model.height - (truncate ((toFloat (model.height - model.partAbove)) * normalizer)) - model.partBelow


                        width =
                            (model.width - 20  - model.partLeft - model.partRight) // (List.length model.data) - margin

                        xCoor =
                            (toFloat (width + margin)) * ((toFloat iter) + 1 / 2) + (toFloat model.partLeft)

                        delay =
                            (toString ((toFloat iter) / 24)) ++ "s"

                        start =
                            (toString (model.height - model.partBelow))

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
