module Swole.Types.Workout
    exposing
        ( Workout
        , addGroup
        , deleteGroup
        , new
        , workoutGroups
        )

import Lens exposing (Lens)
import List.Extra as List
import Swole.Types.Group as Group exposing (Group)


type Workout
    = Workout (List Group)


new : Workout
new =
    Workout []


addGroup : Workout -> Workout
addGroup workout =
    workout
        |> workoutGroups.get
        |> flip List.append [ Group.new ]
        |> flip workoutGroups.set workout


deleteGroup : Int -> Workout -> Workout
deleteGroup i (Workout gs) =
    gs
        |> List.removeAt i
        |> Workout


workoutGroups : Lens Workout (List Group)
workoutGroups =
    let
        get (Workout gs) =
            gs

        set gs _ =
            Workout gs
    in
    Lens get set
