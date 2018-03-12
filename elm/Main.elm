module Main exposing (main)

import Html exposing (Html, program)
import Swole.Components.Workout as Workout
import Swole.Types.Workout as Workout exposing (Workout)


type alias Model =
    Workout


type Msg
    = ModelMsg Workout.Msg


view : Model -> Html Msg
view model =
    Html.map ModelMsg <| Workout.view model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelMsg m ->
            let
                ( updated, cmd ) =
                    Workout.update m model
            in
            ( updated, Cmd.map ModelMsg cmd )


main : Program Never Model Msg
main =
    program
        { init = ( Workout.new, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
