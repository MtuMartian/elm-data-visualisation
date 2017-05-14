module PieChart exposing (..)

import Array
import Attributes exposing (onCreate)
import Color exposing (Color, rgb)
import Dict exposing (..)
import Html exposing (Html, div, text)
import Model exposing (..)
import Msgs exposing (..)
import ChartingMessages exposing (..)
import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


dataSum : List PieDataModel -> Float
dataSum slices =
    List.map (\slice -> slice.value) slices
        |> List.sum

defaultModelWithData : List PieDataModel -> String -> PieModel
defaultModelWithData data id =
  { id = id
  , data = data
  , radius = 200
  }

defaultData : List PieDataModel
defaultData =
  [ { id = 1, value = 1, label = "s1" }
  , { id = 2, value = 1, label = "s2" }
  , { id = 3, value = 3, label = "s3" }
  ]

-- VIEW

view : List PieDataModel -> ChartModel -> String -> Html Msg
view data mdl id =
  let
    model =
      Dict.get id mdl.pieCharts
  in
    case model of
      Just model ->
        div [ onCreate (Msgs.Msg_ (PieChartCreated id model)) ]
            [ svg
                [ width (toString (model.radius * 2)), height (toString (model.radius * 2)), viewBox ("0 0 " ++ (toString (model.radius * 2)) ++ " " ++ (toString (model.radius * 2))) ]
                (slices model 0 0)
            ]
      Nothing ->
        let
          model =
            defaultModelWithData data id
        in
          div [ onCreate (Msgs.Msg_ (PieChartCreated id model )) ] []


slices : PieModel -> Int -> Float -> List (Svg Msg)
slices model iter prev =
    if iter >= (List.length model.data) then
        []
    else
        let
            slice =
                Array.fromList model.data
                    |> Array.get iter
        in
            case slice of
                Just slice ->
                    let
                        radius = toFloat model.radius

                        center =
                            ( radius, radius )

                        normalizedValue =
                            slice.value / (dataSum model.data)

                        start =
                            prev

                        end =
                            prev + (normalizedValue * 360)
                    in
                        Svg.path [ d (arc center radius start end), stroke "white", strokeWidth "1", fill (randColor iter) ] []
                            :: (slices model (iter + 1) end)

                Nothing ->
                    []


randColor : Int -> String
randColor s =
    let
        seed =
            initialSeed s

        gen =
            Random.int 10 99

        rt =
            Random.step gen seed

        r = 10
            --Tuple.first rt

        gt =
            Random.step gen (Tuple.second rt)

        g =
            Tuple.first gt

        bt =
            Random.step gen (Tuple.second gt)

        b =
            Tuple.first bt

        msg =
            Debug.log ((toString r) ++ (toString g) ++ (toString b)) 0
    in
        ("#" ++ (toString r) ++ (toString g) ++ (toString b))



-- Params: center (x, y), radius, angle (rad)


polarToCart : ( Float, Float ) -> Float -> Float -> ( Float, Float )
polarToCart center radius angle =
    let
        angleRad =
            (angle - 90) * pi / 180

        x =
            (Tuple.first center) + (radius * (cos angleRad))

        y =
            (Tuple.second center) + (radius * (sin angleRad))
    in
        ( x, y )



-- Creates an arc with given parameters


arc : ( Float, Float ) -> Float -> Float -> Float -> String
arc center radius start end =
    let
        centerX =
            toString (Tuple.first center)

        centerY =
            toString (Tuple.second center)

        startCoor =
            polarToCart center radius end

        startX =
            toString (Tuple.first startCoor)

        startY =
            toString (Tuple.second startCoor)

        endCoor =
            polarToCart center radius start

        endX =
            toString (Tuple.first endCoor)

        endY =
            toString (Tuple.second endCoor)

        large =
            if (end - start) <= 180 then
                "0"
            else
                "1"

        res =
            String.join
                " "
                ([ "M"
                 , centerX
                 , centerY
                 , "L"
                 , startX
                 , startY
                 , "A"
                 , centerX
                 , centerY
                 , "0"
                 , large
                 , "0"
                 , endX
                 , endY
                 , "Z"
                 ]
                )
    in
        res
