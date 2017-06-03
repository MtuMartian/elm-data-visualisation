module SvgViews exposing (..)

import CSS exposing (box)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Attributes exposing (..)
import Msgs exposing (Msg)
import Html exposing (Html, div, text)
import Html.Attributes as HAttr exposing (style)

label : Float -> Float -> String -> String -> Svg Msg
label xCoor yCoor label value =
  foreignObject
    [ x (toString xCoor)
    , y (toString yCoor)
    ]
    [ div
      [ HAttr.style box ]
      [ Html.b
        []
        [ Html.text (label ++ ": ")]
      , Html.text value
      ]
    ]

boxedForeignObject : Float -> Float -> List (Html Msg) -> Svg Msg
boxedForeignObject xCoor yCoor elem =
  foreignObject
    [ x (toString xCoor)
    , y (toString yCoor)
    ]
    [ div
      [ HAttr.style box ]
      elem
    ]
