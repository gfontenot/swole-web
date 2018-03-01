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
import Helpers.Events exposing (onDeleteWhen)
import Swole.Types.Movement exposing (Movement)

type alias Movements = List (Int, Movement)
type alias Model = Movements

type Msg
    = NoOp
    | MovementChanged (Int, Movement)
    | AddMovement
    | DeleteMovement Int

view : Movements -> Html Msg
view movements =
    div []
        (movementFields movements)

update : Msg -> Movements -> (Movements, Cmd Msg)
update msg movements =
    case msg of
        NoOp ->
            (movements, Cmd.none)

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

movementsAround : Int -> Movements -> (List Movement, List Movement)
movementsAround idx movements
    = movements
    |> List.map Tuple.second
    |> List.splitAt idx
    |> Tuple.mapSecond (List.drop 1)

updateFocus : Int -> Cmd Msg
updateFocus idx =
    let
        fieldId = "movement-" ++ toString idx
        setFocus = Dom.focus fieldId
    in
        Task.attempt (always NoOp) setFocus

movementFields : Movements -> List (Html Msg)
movementFields movements = case movements of
    [] -> [defaultMovementField]
    ms ->
        let fields = List.intersperse plusLabel (List.map movementField ms)
        in
           fields ++ [plusButton]
plusLabel : Html a
plusLabel = text "+"

plusButton : Html Msg
plusButton = button [ onClick AddMovement ] [ text "+" ]

defaultMovementField : Html Msg
defaultMovementField = movementField (0, "")

movementField : (Int, Movement) -> Html Msg
movementField (idx, movement) =
    input
        [ type_ "text"
        , placeholder "movement"
        , value movement
        , id <| "movement-" ++ toString idx
        , onInput <| \m -> MovementChanged (idx, m)
        , onDeleteWhen (movement == "") <| DeleteMovement idx
        ]
        []

splitMovements : (Int, Movement) -> (Int, List Movement)
splitMovements (idx, str)
    = str
    |> String.split "+"
    |> \ms -> ((idx + List.length ms - 1), ms)
