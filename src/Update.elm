module Update exposing (..)

import ChartingMessages exposing (..)
import Debug
import Msgs exposing (Msg(..))
import Model exposing (Model, BarModel)


chartUpdate : ChartMsg -> BarModel -> ( BarModel, Cmd Msg )
chartUpdate msg model =
  case msg of
    BarGraphMouseOver data ->
        let
          testmessage = Debug.log "TESTING" ""
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
    BarGraphMouseOut data ->
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

    ElementCreated type_ id->
      let
        testMessage = Debug.log "something was created" ""
      in
        (model, Cmd.none)
