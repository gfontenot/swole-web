module Swole.Types.Group exposing (Group, addSet, deleteSet, groupComplex, groupSets, new)

import Lens exposing (Lens)
import List.Extra as List
import Swole.Types.Complex as Complex exposing (Complex)
import Swole.Types.Weight exposing (Weight(..))
import Swole.Types.WorkoutSet as WorkoutSet exposing (WorkoutSet)


type Group
    = Group Complex (List WorkoutSet)


new : Group
new =
    Group Complex.new []


addSet : Group -> Group
addSet group =
    let
        count =
            group
                |> groupComplex.get
                |> Complex.movementCount
    in
    group
        |> groupSets.get
        |> flip List.append [ WorkoutSet.new count (Kilos 0) ]
        |> flip groupSets.set group


deleteSet : Int -> Group -> Group
deleteSet i group =
    group
        |> groupSets.get
        |> List.removeAt i
        |> flip groupSets.set group


groupComplex : Lens Group Complex
groupComplex =
    let
        get (Group c _) =
            c

        set c (Group _ ws) =
            Group c ws
    in
    Lens get set


groupSets : Lens Group (List WorkoutSet)
groupSets =
    let
        get (Group _ ws) =
            ws

        set ws (Group c _) =
            Group c ws
    in
    Lens get set
