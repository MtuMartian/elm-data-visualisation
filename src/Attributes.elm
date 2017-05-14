module Attributes exposing (..)

import Html exposing (Attribute)
import Json.Decode as Json
import Html.Events as Events exposing (on)


onCreate : msg -> Attribute msg
onCreate message =
    on "DOMNodeInserted" (Json.succeed message)
