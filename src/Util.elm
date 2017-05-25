module Util exposing (..)

import Dict exposing (..)

tupleToDict : List (String, String) -> Dict String String
tupleToDict tuples =
  List.foldr (\x -> Dict.insert (Tuple.first x) (Tuple.second x)) Dict.empty tuples
