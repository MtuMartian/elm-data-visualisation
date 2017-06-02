module BubbleChart exposing (..)

import Array exposing (get, fromList)
import Attributes exposing (..)
import CSS exposing (box)
import Dict exposing (..)
import Html exposing (Html, div)
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

defaultModelWithData : List BubbleDataModel -> Dict String String -> String -> Int -> Int -> BubbleChartModel
defaultModelWithData data properties id width height =
  { id = id
  , data = data
  , width = width
  , height = height
  , rangeHori = (0, 10)
  , rangeVert = (0, 10)
  , margin = Dict.get "margin" properties |> Maybe.withDefault "10" |> String.toInt |> Result.withDefault 10
  , partLeft = Dict.get "partitionLeft" properties |> Maybe.withDefault "120" |> String.toInt |> Result.withDefault 120
  , partRight = Dict.get "partitionRight" properties |> Maybe.withDefault "120" |> String.toInt |> Result.withDefault 120
  , partBelow = Dict.get "partitionBelow" properties |> Maybe.withDefault "120" |> String.toInt |> Result.withDefault 120
  , partAbove = Dict.get "partitionAbove" properties |> Maybe.withDefault "120" |> String.toInt |> Result.withDefault 120
  , title = Dict.get "title" properties |> Maybe.withDefault "Title"
  , vertTitle = Dict.get "vertTitle" properties |> Maybe.withDefault "Vertical Title"
  , horiTitle = Dict.get "horiTitle" properties |> Maybe.withDefault "Horizontal Title"
  , bubbleSize = Dict.get "bubbleSize" properties |> Maybe.withDefault "10" |> String.toFloat |> Result.withDefault 10
  , ticksVertical = Dict.get "ticksVertical" properties |> Maybe.withDefault "6" |> String.toInt |> Result.withDefault 6
  , ticksHorizontal = Dict.get "ticksHorizontal" properties |> Maybe.withDefault "6" |> String.toInt |> Result.withDefault 6
  }

view : List BubbleDataModel -> List Property -> (Int, Int) -> ChartModel -> String -> Html Msg
view data properties dimensions mdl id =
  let
    model =
      Dict.get id mdl.bubbleCharts
  in
    case model of
      Just model ->
        div [ onCreate (Msgs.Msg_ (BubbleChartCreated id model)) ]
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
              div [ onCreate (Msgs.Msg_ (BubbleChartCreated id model)) ] []


axes : BubbleChartModel -> List (Svg Msg)
axes model =
  ([ line
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
      , y1 (toString model.partAbove)
      , y2 (toString (model.height - model.partBelow))
      , strokeWidth "2"
      , stroke "black"
      ]
      []
  ]
  ++
  (crossSections model)
  ++
  (titles model)
  ++
  (bubbles model 0 [])
  )

titles : BubbleChartModel -> List (Svg Msg)
titles model =
  (title model) ++ (vertTitle model) ++ (horiTitle model)

title : BubbleChartModel -> List (Svg Msg)
title model =
  let
    xCoor = model.width // 2 |> toString
    yCoor = model.partAbove // 2 |> toString
  in
    [ Svg.text_
      [ x xCoor, y yCoor, textAnchor "middle", alignmentBaseline "middle" ]
      [ Svg.text model.title ]
    ]

vertTitle : BubbleChartModel -> List (Svg Msg)
vertTitle model =
  let
    contentHeight = model.height - model.partBelow - model.partAbove
    xCoor = model.partLeft // 2 |> toString
    yCoor = contentHeight // 2 + model.partAbove |> toString
  in
    [ Svg.text_
        [ x xCoor, y yCoor, width (toString model.partLeft), textAnchor "middle", alignmentBaseline "middle" ]
        [ Svg.text model.vertTitle ]
    ]

horiTitle : BubbleChartModel -> List (Svg Msg)
horiTitle model =
  let
    contentWidth = model.width - model.partLeft - model.partRight
    xCoor = contentWidth // 2 + model.partLeft |> toString
    yCoor = model.height - model.partBelow // 2 |> toString

  in
    [ Svg.text_
        [ x xCoor, y yCoor, textAnchor "middle", alignmentBaseline "middle" ]
        [ Svg.text model.horiTitle ]
    ]

crossSections : BubbleChartModel -> List (Svg Msg)
crossSections model =
  (horiCrossSections model 0)
  ++
  (vertCrossSections model 0)

horiCrossSections : BubbleChartModel -> Int -> List (Svg Msg)
horiCrossSections model iter =
  if iter >= model.ticksHorizontal then
    []
  else
    let
      total = model.ticksHorizontal |> toFloat

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

      min = Tuple.first model.rangeVert
      max = Tuple.second model.rangeVert

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


vertCrossSections : BubbleChartModel -> Int -> List (Svg Msg)
vertCrossSections model iter =
  if iter >= model.ticksVertical then
    []
  else
    let
      total = model.ticksVertical |> toFloat

      contentWidth = model.width - model.partLeft - model.partRight
      contentHeight = model.height - model.partAbove - model.partBelow

      height =
        contentHeight |> toFloat

      width =
        contentWidth |> toFloat

      xCoor =
        width / total * (toFloat iter) + (toFloat model.partLeft)
          |> toString

      min = Tuple.first model.rangeVert
      max = Tuple.second model.rangeVert

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

bubbles : BubbleChartModel -> Int -> List (Svg Msg) -> List (Svg Msg)
bubbles model iter infoBoxes =
  if iter >= (List.length model.data) then
    infoBoxes
  else
    let
      bubble =
        Array.get iter (Array.fromList (model.data))
    in
      case bubble of
        Just bubble ->
          let

            minHori = Tuple.first model.rangeHori
            maxHori = Tuple.second model.rangeHori

            minVert = Tuple.first model.rangeVert
            maxVert = Tuple.second model.rangeVert

            normalizerHori =
              (bubble.valueHori - minHori) / (maxHori - minHori)

            normalizerVert =
              (bubble.valueVert - minVert) / (maxVert - minVert)

            contentHeight =
              model.height - model.partAbove - model.partBelow |> toFloat

            contentWidth =
              model.width - model.partLeft - model.partRight |> toFloat

            bubbleXF = contentWidth * normalizerHori + (toFloat model.partLeft)
            bubbleYF = (toFloat model.height) - contentHeight * normalizerVert - (toFloat model.partBelow)

            bubbleX = bubbleXF |> toString
            bubbleY = bubbleYF |> toString

            radiusMod =
              if bubble.isHighlighted then
                2
              else
                1

            bubbleRadius = model.bubbleSize * radiusMod |> toString

            infoBox = label bubbleXF bubbleYF bubble.label (toString bubble.value)

            updatedInfoBoxes =
              if bubble.isHighlighted then
                infoBoxes ++ [ infoBox ]
              else
                infoBoxes

          in
            ([ circle
                [ cx bubbleX
                , cy bubbleY
                , r bubbleRadius
                , fill "blue"
                , onMouseOver (Msgs.Msg_ (BubbleChartMouseOver bubble model.id))
                , onMouseOut (Msgs.Msg_ (BubbleChartMouseOut bubble model.id))
                ]
                [ ]]
            ++ (bubbles model (iter + 1) updatedInfoBoxes)
            )
        Nothing ->
          []
