module Update exposing (..)

import Debug
import Msgs exposing (Msg(..))
import Model exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataClicked ->
            let
                test =
                    Debug.log "data clicked" 0

                updatedModel =
                    { model | data = [] }
            in
                ( updatedModel, Cmd.none )

        DataMouseOver data ->
            let
                updatedData =
                    List.map
                        (\x ->
                            if x.id == data.id then
                                { x | isHighlighted = True }
                            else
                                x
                        )
                        model.data

                updatedModel =
                    { model | data = updatedData }
            in
                ( updatedModel, Cmd.none )

        DataMouseExit data ->
            let
                updatedData =
                    List.map
                        (\x ->
                            if x.id == data.id then
                                { x | isHighlighted = False }
                            else
                                x
                        )
                        model.data

                updatedModel =
                    { model | data = updatedData }
            in
                ( updatedModel, Cmd.none )
