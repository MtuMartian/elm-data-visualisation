module BarGraph exposing (..)

import Array
import Html exposing (Html, div, text)
import Model exposing (..)
import Msgs exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events exposing (onClick, onMouseOver, onMouseOut)


-- MODEL
{--type alias Model =
  { bars : List BarModel
  , label : String
  , width : Int
  , height : Int
  , range : ( Float, Float )
  }

type alias BarModel =
  { id : Int
  , value : Float
  , label : String
  }
--}
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width (toString model.width), height (toString model.height), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)) ]
            ((axes model) ++ (bars model 0))
        ]


axes : Model -> List (Svg Msg)
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


ticks : Model -> Int -> Int -> List (Svg Msg)
ticks model iter total =
    if iter >= total then
        []
    else
        let
            height =
                toString (model.height * iter // total)
        in
            (line [ x1 "0", x2 "20", y1 height, y2 height, strokeWidth "2", stroke "black" ] []) :: (ticks model (iter + 1) total)


bars : Model -> Int -> List (Svg Msg)
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
                                    , onMouseOver (DataMouseOver bar)
                                    , onMouseOut (DataMouseExit bar)
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
