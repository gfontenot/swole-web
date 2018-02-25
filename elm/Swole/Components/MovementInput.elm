module Swole.Components.MovementInput exposing
    ( Msg
    , Model
    , view
    , update
    )

import Html exposing (Html, program, div, input, text, button)
import Html.Attributes exposing (placeholder, type_, value, id)
import Html.Events exposing (onClick, onInput)
import Task
import Dom exposing (focus)
import List.Extra as List

import Helpers.List as List
import Helpers.Events exposing (onDelete)
import Swole.Types.Movement exposing (Movement)

type alias Movements = List (Int, Movement)
type alias Model = Movements

type Msg
    = MovementChanged (Int, Movement)
    | AddMovement
    | DeleteMovement Int
    | FocusedField

view : Movements -> Html Msg
view movements =
    div []
        (movementFields movements)

update : Msg -> Movements -> (Movements, Cmd Msg)
update msg movements =
    case msg of
        MovementChanged (idx, m) ->
            let
                (newIdx, newMovements) = splitMovements (idx, m)
                (before, after) = movementsAround idx movements
                allMovements = before ++ newMovements ++ after
            in
                (List.enumerated allMovements, updateFocus newIdx)

        AddMovement ->
            let
                rawMovements = List.map Tuple.second movements
                idx = List.length rawMovements
            in
                (List.enumerated <| rawMovements ++ [""], updateFocus idx)

        DeleteMovement idx ->
            let
                newMovements
                    = movements
                    |> List.map Tuple.second
                    |> List.removeAt idx

                newIdx = max 0 (idx - 1)
            in
               (List.enumerated newMovements, updateFocus newIdx)

        FocusedField ->
            (movements, Cmd.none)

movementsAround : Int -> Movements -> (List Movement, List Movement)
movementsAround idx movements
    = movements
    |> List.map Tuple.second
    |> List.splitAt idx
    |> Tuple.mapSecond (List.drop 1)

updateFocus : Int -> Cmd Msg
updateFocus idx =
    let
        checkFocus : Result Dom.Error () -> Msg
        checkFocus result = case result of
            Ok _ -> FocusedField
            Err _ -> FocusedField
    in
        Task.attempt checkFocus (Dom.focus <| "movement-" ++ toString idx)

movementFields : Movements -> List (Html Msg)
movementFields movements = case movements of
    [] -> [movementField (0, "")]
    ms ->
        let fields = List.intersperse plusLabel (List.map movementField ms)
        in
           fields ++ [plusButton]

plusLabel : Html Msg
plusLabel = text "+"

plusButton : Html Msg
plusButton = button [ onClick AddMovement ] [ text "+" ]

movementField : (Int, Movement) -> Html Msg
movementField (idx, movement) =
    input
        [ type_ "text"
        , placeholder "movement"
        , value movement
        , id <| "movement-" ++ toString idx
        , onInput <| \m -> MovementChanged (idx, m)
        , onDelete <| DeleteMovement idx
        ]
        []

splitMovements : (Int, Movement) -> (Int, List Movement)
splitMovements (idx, str)
    = str
    |> String.split "+"
    |> List.map String.trim
    |> \ms -> ((idx + List.length ms - 1), ms)

main : Program Never Movements Msg
main = program
    { init = ([], Cmd.none)
    , update = update
    , subscriptions = (always Sub.none)
    , view = view
    }
