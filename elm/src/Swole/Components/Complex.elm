module Swole.Components.Complex
    exposing
        ( Msg
        , update
        , view
        )

import Dom exposing (focus)
import Extensions.Events exposing (onDeleteWhen)
import Html exposing (Html, button, div, input, program, text)
import Html.Attributes exposing (id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Swole.Components.Common exposing (plusLabel)
import Swole.Types.Complex as Complex exposing (Complex)
import Swole.Types.Movement exposing (Movement)
import Task


type Msg
    = NoOp
    | MovementChanged Int Movement
    | AddMovement
    | DeleteMovement Int


view : Complex -> Html Msg
view movements =
    div []
        (movementFields movements)


update : Msg -> Complex -> ( Complex, Cmd Msg )
update msg complex =
    case msg of
        NoOp ->
            ( complex, Cmd.none )

        MovementChanged idx m ->
            let
                addedComplex =
                    Complex.fromMovement m

                newComplex =
                    Complex.insertAt idx addedComplex complex

                newIdx =
                    idx + Complex.movementCount addedComplex - 1
            in
            ( newComplex, updateFocus newIdx )

        AddMovement ->
            let
                newIdx =
                    Complex.movementCount complex
            in
            ( complex ++ [ "" ], updateFocus newIdx )

        DeleteMovement idx ->
            let
                newMovements =
                    Complex.removeAt idx complex

                newIdx =
                    max 0 (idx - 1)
            in
            ( newMovements, updateFocus newIdx )


updateFocus : Int -> Cmd Msg
updateFocus idx =
    let
        fieldId =
            "movement-" ++ toString idx

        setFocus =
            Dom.focus fieldId
    in
    Task.attempt (always NoOp) setFocus


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
