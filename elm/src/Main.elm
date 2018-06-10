module Main exposing (main)

import Complex.State as Complex
import Complex.Types as Complex exposing (Complex)
import Complex.View as Complex
import Extensions.Maybe as Maybe
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Swole.Components.WorkoutSet as WorkoutSet
import Swole.Types.WorkoutSet as WorkoutSet exposing (WorkoutSet)
import Weight.Types exposing (Weight(..))


type Msg
    = ComplexChanged Complex.Msg
    | SetChanged Int WorkoutSet.Msg
    | AddSet
    | DeleteSet Int


type alias Model =
    { complex : Complex
    , sets : List WorkoutSet
    }


initialModel : Model
initialModel =
    { complex = Complex.new
    , sets = []
    }


view : Model -> Html Msg
view model =
    div []
        [ map ComplexChanged <| Complex.view model.complex
        , ol [] <| List.indexedMap viewSet model.sets
        , button [ onClick AddSet ] [ text "Add set" ]
        ]


viewSet : Int -> WorkoutSet -> Html Msg
viewSet idx set =
    li []
        [ Html.map (SetChanged idx) (WorkoutSet.view set)
        , button [ onClick (DeleteSet idx) ] [ text "Delete" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetChanged idx m ->
            let
                updated =
                    model.sets
                        |> List.updateAt idx (WorkoutSet.update m)
                        |> Maybe.fromJust
            in
            ( { model | sets = updated }, Cmd.none )

        ComplexChanged msg ->
            let
                ( updated, cmd ) =
                    Complex.update msg model.complex
            in
            ( { model | complex = updated }, Cmd.map ComplexChanged cmd )

        AddSet ->
            let
                count =
                    Complex.movementCount model.complex

                updated =
                    model.sets ++ [ WorkoutSet.new count (Kilos 0) ]
            in
            ( { model | sets = updated }, Cmd.none )

        DeleteSet i ->
            let
                updated =
                    List.removeAt i model.sets
            in
            ( { model | sets = updated }, Cmd.none )


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
