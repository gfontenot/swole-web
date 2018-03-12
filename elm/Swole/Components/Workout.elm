module Swole.Components.Workout exposing (Msg, update, view)

import Helpers.Maybe as Maybe
import Html exposing (Html, button, div, li, ol, text)
import Html.Events exposing (onClick)
import List.Extra as List
import Swole.Components.Group as Group
import Swole.Types.Group as Group exposing (Group)
import Swole.Types.Workout as Workout exposing (Workout, workoutGroups)


type Msg
    = GroupChanged Int Group.Msg
    | AddGroup
    | DeleteGroup Int


view : Workout -> Html Msg
view workout =
    div []
        [ ol [] <| List.indexedMap viewGroup <| workoutGroups.get workout
        , button [ onClick AddGroup ] [ text "Add set" ]
        ]


viewGroup : Int -> Group -> Html Msg
viewGroup idx group =
    li []
        [ Html.map (GroupChanged idx) (Group.view group) ]


update : Msg -> Workout -> ( Workout, Cmd Msg )
update msg workout =
    case msg of
        GroupChanged idx m ->
            let
                ( newGroup, cmd ) =
                    workout
                        |> workoutGroups.get
                        |> List.getAt idx
                        |> Maybe.fromJust
                        |> Group.update m

                updated =
                    workout
                        |> workoutGroups.get
                        |> List.setAt idx newGroup
                        |> Maybe.fromJust
                        |> flip workoutGroups.set workout
            in
            ( updated, Cmd.map (GroupChanged idx) cmd )

        AddGroup ->
            let
                updated =
                    Workout.addGroup workout
            in
            ( updated, Cmd.none )

        DeleteGroup idx ->
            let
                updated =
                    workout
                        |> Workout.deleteGroup idx
            in
            ( updated, Cmd.none )
