module PieChart exposing (..)

import Array
import Color exposing (Color, rgb)
import Html exposing (Html, div, text)
import Model exposing (..)
import Msgs exposing (..)
import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


dataSum : List DataModel -> Float
dataSum slices =
  List.map (\slice -> slice.value) slices
    |> List.sum

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ svg
      [ width (toString model.width), height (toString model.height), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height)) ]
      (slices model 0 0)
    ]

slices : Model -> Int -> Float -> List (Svg Msg)
slices model iter prev =
  if iter >= (List.length model.data) then
    []
  else
    let
      slice = Array.fromList model.data
        |> Array.get iter
    in
      case slice of
        Just slice ->
          let
            radius = (Basics.min model.width model.height |> toFloat) / 2
            center = (radius, radius)
            normalizedValue = slice.value / (dataSum model.data)
            start = prev
            end = prev + (normalizedValue * 360)
          in
            Svg.path [ d (arc center radius start end), stroke "transparent", fill (randColor iter)] []
            :: (slices model (iter + 1) end)
        Nothing ->
          []

randColor : Int -> String
randColor s =
  let
    seed = initialSeed s
    gen = Random.int 20 99
    rt = Random.step gen seed
    r = Tuple.first rt
    gt = Random.step gen (Tuple.second rt)
    g = Tuple.first gt
    bt = Random.step gen (Tuple.second gt)
    b = Tuple.first bt
    msg = Debug.log ((toString r) ++ (toString g) ++ (toString b)) 0
  in
    ("#" ++ (toString r) ++ (toString g) ++ (toString b))

-- Params: center (x, y), radius, angle (rad)
polarToCart : (Float, Float) -> Float -> Float -> (Float, Float)
polarToCart center radius angle =
  let
    angleRad = (angle - 90) * pi / 180
    x = (Tuple.first center) + (radius * (cos angleRad))
    y = (Tuple.second center) + (radius * (sin angleRad))
  in
    (x, y)

-- Creates an arc with given parameters
arc : (Float, Float) -> Float -> Float -> Float -> String
arc center radius start end =
  let
    centerX = toString (Tuple.first center)
    centerY = toString (Tuple.second center)

    startCoor = polarToCart center radius end
    startX = toString (Tuple.first startCoor)
    startY = toString (Tuple.second startCoor)

    endCoor = polarToCart center radius start
    endX = toString (Tuple.first endCoor)
    endY = toString (Tuple.second endCoor)

    large =
      if (end - start) <= 180 then
        "0"
      else
        "1"

    res = String.join
        " "
        (["M", centerX, centerY
        , "L", startX, startY
        , "A", centerX, centerY, "0", large, "0", endX, endY
        , "Z"])
  in
    res
