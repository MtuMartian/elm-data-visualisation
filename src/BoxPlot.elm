module BoxPlot exposing (..)

import Array
import Html exposing (Html, div, text)
import Model exposing (..)
import Msgs exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events exposing (onClick, onMouseOver, onMouseOut)


view : Model -> Html Msg
view model =
    div
        []
        [ svg
            [ width (toString model.width), height (toString model.height), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)) ]
            (boxPlot model)
        ]


boxPlot : Model -> List (Svg Msg)
boxPlot model =
    List.concat
        [ drawMinMax model
        , drawMedianLine model
        , drawBox model
        ]


drawMinMax : Model -> List (Svg Msg)
drawMinMax model =
    let
        margin =
            10

        maxY =
            toString margin

        minY =
            toString (model.height - margin)

        xStart =
            toString margin

        xEnd =
            toString (model.width - margin)
    in
        [ line
            [ x1 xStart
            , x2 xEnd
            , y1 maxY
            , y2 maxY
            , strokeWidth "2"
            , stroke "black"
            ]
            []
        , line
            [ x1 xStart
            , x2 xEnd
            , y1 minY
            , y2 minY
            , strokeWidth "2"
            , stroke "black"
            ]
            []
        ]


drawMedianLine : Model -> List (Svg Msg)
drawMedianLine model =
    let
        margin =
            10

        max =
            List.map (\x -> x.value) model.data
                |> List.maximum

        min =
            List.map (\x -> x.value) model.data
                |> List.minimum

        med =
            median model.data
    in
        case max of
            Just max ->
                case min of
                    Just min ->
                        case med of
                            Just med ->
                                let
                                    range =
                                        Debug.log "RANGE: " (max - min)

                                    maprint =
                                        Debug.log "Max: " max

                                    miprint =
                                        Debug.log "Min: " min

                                    meprint =
                                        Debug.log "Med: " med

                                    normalized =
                                        (med - min) / range

                                    margin =
                                        10

                                    xStart =
                                        toString (margin * 4)

                                    xEnd =
                                        toString (model.width - (margin * 4))

                                    y =
                                        toString ((toFloat model.height) - (toFloat model.height) * (normalized))
                                in
                                    [ line
                                        [ x1 xStart
                                        , x2 xEnd
                                        , y1 y
                                        , y2 y
                                        , strokeWidth "2"
                                        , stroke "black"
                                        ]
                                        []
                                    ]

                            Nothing ->
                                []

                    Nothing ->
                        []

            Nothing ->
                []


drawBox : Model -> List (Svg Msg)
drawBox model =
    let
        sorted =
            List.sortBy .value model.data

        middle =
            (List.length sorted) // 2

        part1 =
            List.take middle sorted

        part2 =
            List.drop middle sorted

        med1 =
            median part1

        med2 =
            median part2

        max =
            List.map (\x -> x.value) model.data
                |> List.maximum

        min =
            List.map (\x -> x.value) model.data
                |> List.minimum

        med =
            median model.data
    in
        case med1 of
            Just med1 ->
                case med2 of
                    Just med2 ->
                        case max of
                            Just max ->
                                case min of
                                    Just min ->
                                        let
                                            m1debug =
                                                Debug.log "First quartile: " med1

                                            m2debug =
                                                Debug.log "Third quartile: " med2

                                            margin =
                                                10

                                            range =
                                                max - min

                                            normalized1 =
                                                (med1 - min) / range

                                            normalized2 =
                                                (med2 - min) / range

                                            xStart =
                                                margin * 4

                                            xEnd =
                                                model.width - (margin * 4)

                                            rectWidth =
                                                toString (xEnd - xStart)

                                            rectX =
                                                toString ((xEnd + xStart) // 2)

                                            yStart =
                                                (toFloat model.height) - (toFloat model.height) * (normalized1) + margin

                                            yEnd =
                                                (toFloat model.height) - (toFloat model.height) * (normalized2) + margin

                                            rectHeight =
                                                toString (yStart - yEnd)

                                            rectY =
                                                toString ((yEnd + yStart) / 2)

                                            xMid =
                                                toString (model.width // 2)
                                        in
                                            [ rect
                                                [ x (toString xStart)
                                                , y (toString yEnd)
                                                , width rectWidth
                                                , height rectHeight
                                                , fill "transparent"
                                                , stroke "black"
                                                , strokeWidth "2"
                                                ]
                                                []
                                            , line
                                                [ x1 xMid
                                                , x2 xMid
                                                , y1 (toString margin)
                                                , y2 (toString yEnd)
                                                , stroke "black"
                                                , strokeWidth "2"
                                                ]
                                                []
                                            , line
                                                [ x1 xMid
                                                , x2 xMid
                                                , y1 (toString (model.height - margin))
                                                , y2 (toString yStart)
                                                , stroke "black"
                                                , strokeWidth "2"
                                                ]
                                                []
                                            ]

                                    Nothing ->
                                        []

                            Nothing ->
                                []

                    Nothing ->
                        []

            Nothing ->
                []


median : List DataModel -> Maybe Float
median data =
    if (List.length data) % 2 == 1 then
        let
            sorted =
                List.sortBy .value data

            index =
                (List.length sorted) // 2

            median =
                Array.get index (Array.fromList sorted)
        in
            case median of
                Just median ->
                    Just median.value

                Nothing ->
                    Nothing
    else
        let
            sorted =
                List.sortBy .value data

            index1 =
                (List.length sorted) // 2

            index2 =
                (List.length sorted) // 2 - 1

            num1 =
                Array.get index1 (Array.fromList sorted)

            num2 =
                Array.get index2 (Array.fromList sorted)
        in
            case num1 of
                Just num1 ->
                    case num2 of
                        Just num2 ->
                            Just ((num1.value + num2.value) / 2)

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
