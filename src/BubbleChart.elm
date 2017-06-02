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
      [ x1 (toString model.margin)
      , x2 (toString (model.width - model.margin))
      , y1 (toString (model.height - model.margin))
      , y2 (toString (model.height - model.margin))
      , strokeWidth "2"
      , stroke "black"
      ]
      []
  , line
      [ x1 (toString model.margin)
      , x2 (toString model.margin)
      , y1 (toString model.margin)
      , y2 (toString (model.height - model.margin))
      , strokeWidth "2"
      , stroke "black"
      ]
      []
  ]
  ++
  (horiCrossSections model 0)
  ++
  (vertCrossSections model 0)
  ++
  (bubbles model 0 [])
  )

horiCrossSections : BubbleChartModel -> Int -> List (Svg Msg)
horiCrossSections model iter =
  if iter >= model.ticksHorizontal then
    []
  else
    let
      total = model.ticksHorizontal |> toFloat

      height =
        model.height - model.margin * 2 |> toFloat

      width =
        model.width - model.margin |> toFloat

      yCoor =
        height / total * (toFloat iter) + (toFloat model.margin)
          |> toString

    in
      [ line
        [ x1 (toString model.margin)
        , x2 (toString (model.width - model.margin))
        , y1 yCoor
        , y2 yCoor
        , strokeWidth "1"
        , stroke "gray"
        ]
        []
      ] ++ (horiCrossSections model (iter + 1))


vertCrossSections : BubbleChartModel -> Int -> List (Svg Msg)
vertCrossSections model iter =
  if iter >= model.ticksVertical then
    []
  else
    let
      total = model.ticksVertical |> toFloat

      height =
        model.height - model.margin * 2 |> toFloat

      width =
        model.width - model.margin |> toFloat

      xCoor =
        width / total * (toFloat iter) + (toFloat model.margin)
          |> toString

    in
      [ line
        [ x1 xCoor
        , x2 xCoor
        , y1 (toString model.margin)
        , y2 (toString (model.height - model.margin))
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
              model.height - model.margin * 2 |> toFloat

            contentWidth =
              model.width - model.margin * 2 |> toFloat

            bubbleXF = contentWidth * normalizerHori + (toFloat model.margin)
            bubbleYF = (toFloat model.height) - contentHeight * normalizerVert - (toFloat model.margin)

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
