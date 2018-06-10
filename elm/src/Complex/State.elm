module Complex.State exposing (init, subscriptions, update)

import Complex.Types as Complex exposing (Complex, Msg(..))
import Dom exposing (focus)
import Task


init : ( Complex, Cmd Msg )
init =
    ( [], Cmd.none )


subscriptions : Complex -> Sub Msg
subscriptions =
    always Sub.none


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
