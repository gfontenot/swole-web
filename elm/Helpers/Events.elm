module Helpers.Events exposing (onChange, onDeleteEmpty)

import Html exposing (Attribute)
import Html.Events exposing (on, targetValue, keyCode)
import Json.Decode as Json exposing (map)

onChange : (String -> msg) -> Attribute msg
onChange tagger =
  on "change" <| map tagger targetValue

onDeleteEmpty : String -> msg -> Attribute msg
onDeleteEmpty str m =
    let
        checker : Int -> Json.Decoder msg
        checker code = case (code == 8, str == "") of
            (True, True) -> Json.succeed m
            (True, False) -> Json.fail "Text not empty"
            (False, True) -> Json.fail "Wrong key pressed"
            (False, False) -> Json.fail "Wrong key pressed, text not empay"
    in
       on "keydown" <| Json.andThen checker keyCode
