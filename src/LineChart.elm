module LineChart exposing (..)

import Array
import Attributes exposing (..)
import CSS exposing (box)
import Dict exposing (..)
import Debug exposing (log)
import Html exposing (Html, div, text)
import Html.Attributes as HAttr exposing (style)
import Model exposing (..)
import Msgs exposing (Msg)
import ChartingMessages exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events exposing (onMouseOver, onMouseOut)
import SvgViews exposing (label)
import Properties exposing (..)

import Util exposing (tupleToDict)

defaultModelWithData : List LineDataModel -> Dict String String -> String -> Int -> Int -> LineChartModel
defaultModelWithData data properties id width height =
  { id = id
  , data = data
  , width = width
  , height = height
  , rangeX = (0, 15)
  , rangeY = (0, 15)
  , partLeft = Dict.get "partitionLeft" properties |> Maybe.withDefault "0" |> String.toInt |> Result.withDefault 0
  , partRight = Dict.get "partitionRight" properties |> Maybe.withDefault "0" |> String.toInt |> Result.withDefault 0
  , partAbove = Dict.get "partitionAbove" properties |> Maybe.withDefault "0" |> String.toInt |> Result.withDefault 0
  , partBelow = Dict.get "partitionBelow" properties |> Maybe.withDefault "0" |> String.toInt |> Result.withDefault 0
  , ticksX = 4
  , ticksY = 4
  , title = Dict.get "title" properties |> Maybe.withDefault ""
  , titleY = Dict.get "vertTitle" properties |> Maybe.withDefault ""
  , titleX = Dict.get "horiTitle" properties |> Maybe.withDefault ""
  }

view : List LineDataModel -> List Property -> (Int, Int) -> ChartModel -> String -> Html Msg
view data properties dimensions mdl id =
  let
    model =
      Dict.get id mdl.lineCharts
  in
    case model of
      Just model ->
        div [ onCreate (Msgs.Msg_ (LineChartCreated id model)) ]
            [ svg
              [ height (toString model.height), width (toString model.width), viewBox ("0 0 " ++ (toString model.width) ++ " " ++ (toString model.height))]
              ( (axes model) ++ [rect [width (toString model.width), height (toString model.height), fill "none", stroke "black", strokeWidth "2"] []] )
            ]
      Nothing ->
        let
          propertyDict = tupleToDict properties
          model =
            defaultModelWithData data propertyDict id (Tuple.first dimensions) (Tuple.second dimensions)
        in
          div [ onCreate (Msgs.Msg_ (LineChartCreated id model)) ]
              []

axes : LineChartModel -> List (Svg Msg)
axes model =
  [ line
      [ x1 (toString model.partLeft)
      , x2 (toString (model.width - model.partRight))
      , y1 (toString (model.height - model.partBelow))
      , y2 (toString (model.height - model.partBelow))
      , strokeWidth "2"
      , stroke "black"
      ]
      []
  , line
      [ x1 (toString model.partLeft)
      , x2 (toString model.partLeft)
      , y1 (toString (model.height - model.partBelow))
      , y2 (toString (model.partAbove))
      , strokeWidth "2"
      , stroke "black"
      ]
      []
  ]
  ++ (crossSections model)
  ++ (horiAxisTitle model)
  ++ (vertAxisTitle model)
  ++ (mainTitle model)
  ++ (lines model 0 [])

horiAxisTitle : LineChartModel -> List (Svg Msg)
horiAxisTitle model =
  let
    contentWidth = model.width - model.partLeft - model.partRight
    xCoor = contentWidth // 2 + model.partLeft |> toString
    yCoor = model.height - model.partBelow // 2 |> toString

  in
    [ Svg.text_
      [ x xCoor, y yCoor, textAnchor "middle", alignmentBaseline "middle" ]
      [ Svg.text model.titleX ]
    ]

vertAxisTitle : LineChartModel -> List (Svg Msg)
vertAxisTitle model =
  let
    contentHeight = model.height - model.partBelow - model.partAbove
    xCoor = model.partLeft // 2 |> toString
    yCoor = contentHeight // 2 + model.partAbove |> toString

  in
    [ Svg.text_
      [ x xCoor, y yCoor, width (toString model.partLeft), textAnchor "middle", alignmentBaseline "middle" ]
      [ Svg.text model.titleY ]
    ]

mainTitle : LineChartModel -> List (Svg Msg)
mainTitle model =
  let
    xCoor = model.width // 2 |> toString
    yCoor = model.partAbove // 2 |> toString

  in
    [ Svg.text_
      [ x xCoor, y yCoor, textAnchor "middle", alignmentBaseline "middle" ]
      [ Svg.text model.title ]
    ]

crossSections : LineChartModel -> List (Svg Msg)
crossSections model =
  (horiCrossSections model 0)
  ++
  (vertCrossSections model 0)

horiCrossSections : LineChartModel -> Int -> List (Svg Msg)
horiCrossSections model iter =
  if iter >= model.ticksX then
    []
  else
    let
      total = model.ticksX |> toFloat

      contentWidth = model.width - model.partLeft - model.partRight
      contentHeight = model.height - model.partAbove - model.partBelow

      height =
        contentHeight |> toFloat

      width =
        contentWidth |> toFloat

      yCoor =
        height / total * (toFloat iter)
        --+ (toFloat model.margin)
        + (toFloat model.partAbove)
          |> toString

      min = Tuple.first model.rangeY
      max = Tuple.second model.rangeY

      range =
        max - min

      value =
          min + (range / total * (toFloat iter)) |> toString

    in
      [ line
        [ x1 (toString model.partLeft)
        , x2 (toString (model.width - model.partRight))
        , y1 yCoor
        , y2 yCoor
        , strokeWidth "1"
        , stroke "gray"
        ]
        []
      , Svg.text_
          [ x (toString (model.partLeft - 2)), y yCoor, textAnchor "end", alignmentBaseline "middle" ]
          [Svg.text "" ] --value ]
      ] ++ (horiCrossSections model (iter + 1))


vertCrossSections : LineChartModel -> Int -> List (Svg Msg)
vertCrossSections model iter =
  if iter >= model.ticksY then
    []
  else
    let
      total = model.ticksY |> toFloat

      contentWidth = model.width - model.partLeft - model.partRight
      contentHeight = model.height - model.partAbove - model.partBelow

      height =
        contentHeight |> toFloat

      width =
        contentWidth |> toFloat

      xCoor =
        width / total * (toFloat iter) + (toFloat model.partLeft)
          |> toString

      min = Tuple.first model.rangeY
      max = Tuple.second model.rangeY

      range =
        max - min

      value =
          min + (range / total * (toFloat iter)) |> toString

    in
      [ line
        [ x1 xCoor
        , x2 xCoor
        , y1 (toString model.partAbove)
        , y2 (toString (model.height - model.partBelow))
        , strokeWidth "1"
        , stroke "gray"
        ]
        []
      ] ++ (vertCrossSections model (iter + 1))

lines : LineChartModel -> Int -> List (Svg Msg) -> List (Svg Msg)
lines model iter infoBoxes =
  if iter >= (List.length model.data) - 1 then
    infoBoxes
  else
    let
      point1 =
        Array.get iter (Array.fromList (model.data))
      point2 =
        Array.get (iter + 1) (Array.fromList (model.data))
    in
      case point1 of
        Just point1 ->
          case point2 of
            Just point2 ->
              let
                minX = Tuple.first model.rangeX
                maxX = Tuple.second model.rangeX

                minY = Tuple.first model.rangeY
                maxY = Tuple.second model.rangeY

                normalizerX1 =
                  (point1.valueX - minX) / (maxX - minX)
                normalizerX2 =
                  (point2.valueX - minX) / (maxX - minX)

                normalizerY1 =
                  (point1.valueY - minY) / (maxY - minY)
                normalizerY2 =
                  (point2.valueY - minY) / (maxY - minY)

                contentHeight =
                  model.height - model.partAbove - model.partBelow |> toFloat
                contentWidth =
                  model.width - model.partLeft - model.partRight |> toFloat

                point1X = contentWidth * normalizerX1 + (toFloat model.partLeft)
                point2X = contentWidth * normalizerX2 + (toFloat model.partLeft)

                point1Y = (toFloat model.height) - contentHeight * normalizerY1 - (toFloat model.partBelow)
                point2Y = (toFloat model.height) - contentHeight * normalizerY2 - (toFloat model.partBelow)

              in
                ( [ line
                    [ x1 (toString point1X)
                    , x2 (toString point2X)
                    , y1 (toString point1Y)
                    , y2 (toString point2Y)
                    , strokeWidth "2"
                    , stroke "black"
                    ]
                    []
                  ]
                ++ (lines model (iter + 1) [])
                )
            Nothing ->
              []
        Nothing ->
          []
