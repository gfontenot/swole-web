module Swole.Components.Group exposing (Msg, update, view)

import Helpers.Maybe as Maybe
import Html exposing (Html, button, div, li, ol, program, text)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Swole.Components.Complex as Complex
import Swole.Components.WorkoutSet as WorkoutSet
import Swole.Types.Group as Group exposing (Group, groupComplex, groupSets)
import Swole.Types.WorkoutSet exposing (WorkoutSet)


type Msg
    = ComplexChanged Complex.Msg
    | SetChanged Int WorkoutSet.Msg
    | AddSet
    | DeleteSet Int


view : Group -> Html Msg
view group =
    div []
        [ Html.map ComplexChanged <| Complex.view <| groupComplex.get group
        , ol [] <| List.indexedMap viewSet <| groupSets.get group
        , button [ onClick AddSet ] [ text "Add set" ]
        ]


viewSet : Int -> WorkoutSet -> Html Msg
viewSet idx set =
    li []
        [ Html.map (SetChanged idx) (WorkoutSet.view set)
        , button [ onClick (DeleteSet idx) ] [ text "Delete" ]
        ]


update : Msg -> Group -> ( Group, Cmd Msg )
update msg group =
    case msg of
        SetChanged idx m ->
            let
                updated =
                    group
                        |> groupSets.get
                        |> List.updateAt idx (WorkoutSet.update m)
                        |> Maybe.fromJust
                        |> flip groupSets.set group
            in
            ( updated, Cmd.none )

        ComplexChanged msg ->
            let
                ( updated, cmd ) =
                    group
                        |> groupComplex.get
                        |> Complex.update msg
                        |> Tuple.mapFirst (flip groupComplex.set group)
            in
            ( updated, Cmd.map ComplexChanged cmd )

        AddSet ->
            let
                updated =
                    Group.addSet group
            in
            ( updated, Cmd.none )

        DeleteSet i ->
            let
                updated =
                    group
                        |> Group.deleteSet i
            in
            ( updated, Cmd.none )
