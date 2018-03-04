module Helpers.Events exposing (onChange, onDeleteWhen)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on, targetValue)
import Json.Decode as Json exposing (map)


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" <| map tagger targetValue


onDeleteWhen : Bool -> msg -> Attribute msg
onDeleteWhen condition message =
    let
        isDelete : Int -> Json.Decoder msg
        isDelete code =
            case code == 8 of
                True ->
                    Json.succeed message

                False ->
                    Json.fail "Key pressed wasn't delete"

        checkPrecondition : msg -> Json.Decoder msg
        checkPrecondition message =
            case condition of
                True ->
                    Json.succeed message

                False ->
                    Json.fail "Precondition failed"
    in
    keyCode
        |> Json.andThen isDelete
        |> Json.andThen checkPrecondition
        |> on "keydown"
