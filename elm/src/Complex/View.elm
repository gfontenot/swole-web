module Complex.View exposing (view)

import Complex.Types as Complex exposing (Complex, Msg(..))
import Extensions.Events exposing (onDeleteWhen)
import Html exposing (Html, button, div, input, program, text)
import Html.Attributes exposing (id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Swole.Components.Common exposing (plusLabel)
import Swole.Types.Movement exposing (Movement)


view : Complex -> Html Msg
view movements =
    div []
        (movementFields movements)


movementFields : Complex -> List (Html Msg)
movementFields complex =
    case complex of
        [] ->
            [ defaultMovementField ]

        c ->
            let
                fields =
                    c
                        |> Complex.indexedMap movementField
                        |> List.intersperse plusLabel
            in
            fields ++ [ plusButton ]


plusButton : Html Msg
plusButton =
    button [ onClick AddMovement ] [ text "+" ]


defaultMovementField : Html Msg
defaultMovementField =
    movementField 0 ""


movementField : Int -> Movement -> Html Msg
movementField idx movement =
    input
        [ type_ "text"
        , placeholder "movement"
        , value movement
        , id <| "movement-" ++ toString idx
        , onInput <| MovementChanged idx
        , onDeleteWhen (movement == "") <| DeleteMovement idx
        ]
        []
