module Helpers.Events exposing (onChange, onDelete)

import Html exposing (Attribute)
import Html.Events exposing (on, targetValue, keyCode)
import Json.Decode as Json exposing (map)

onChange : (String -> msg) -> Attribute msg
onChange tagger =
  on "change" <| map tagger targetValue

onDelete : msg -> Attribute msg
onDelete m =
    let
        isDelete code = case code == 8 of
            True -> Json.succeed m
            False -> Json.fail "Key pressed wasn't delete"
    in
        keyCode
        |> Json.andThen isDelete
        |> on "keydown"
