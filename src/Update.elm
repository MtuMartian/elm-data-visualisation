module Update exposing (..)

import ChartingMessages exposing (..)
import Debug
import Dict exposing (..)
import Msgs exposing (Msg(..))
import Model exposing (Model, ChartModel, BarModel)


chartUpdate : ChartMsg -> ChartModel -> ( ChartModel, Cmd Msg )
chartUpdate msg model =
    case msg of
        BarGraphMouseOver data id ->
            let
                barModel =
                    Dict.get id model.barGraphs
            in
                case barModel of
                    Just barModel ->
                        let
                            updatedData =
                                List.map
                                    (\x ->
                                        if x.id == data.id then
                                            { x | isHighlighted = True }
                                        else
                                            x
                                    )
                                    barModel.data

                            updatedBarModel =
                                { barModel | data = updatedData }

                            updatedDict =
                                Dict.insert id updatedBarModel model.barGraphs
                        in
                            ( { model | barGraphs = updatedDict }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        BarGraphMouseOut data id ->
            let
                barModel =
                    Dict.get id model.barGraphs
            in
                case barModel of
                    Just barModel ->
                        let
                            updatedData =
                                List.map
                                    (\x ->
                                        if x.id == data.id then
                                            { x | isHighlighted = False }
                                        else
                                            x
                                    )
                                    barModel.data

                            updatedBarModel =
                                { barModel | data = updatedData }

                            updatedDict =
                                Dict.insert id updatedBarModel model.barGraphs
                        in
                            ( { model | barGraphs = updatedDict }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        BarGraphCreated id barModel ->
            let
                updatedDict =
                    Dict.insert id barModel model.barGraphs
            in
                ( { model | barGraphs = updatedDict }, Cmd.none )

        PieChartCreated id pieModel ->
            let updatedDict =
                    Dict.insert id pieModel model.pieCharts
            in
                ( { model | pieCharts = updatedDict }, Cmd.none )

        BoxPlotCreated id boxPlotModel ->
            let updatedDict =
                    Dict.insert id boxPlotModel model.boxPlots
            in
                ( { model | boxPlots = updatedDict }, Cmd.none )
