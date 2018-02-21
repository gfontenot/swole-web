module Swole.Components.MovementInput exposing
    ( main )

import Html exposing (Html, beginnerProgram, div, input, text, button)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List

import Helpers.List as List

type alias Movement = String
type alias Movements = List (Int, Movement)

type Msg
    = MovementChanged (Int, Movement)
    | AddMovement

view : Movements -> Html Msg
view movements =
    div []
        (movementFields movements)

update : Msg -> Movements -> Movements
update msg movements =
    case msg of
        MovementChanged (idx, m) ->
            let
                newMovements = splitMovements m
                (before, after) = movementsAround idx movements
                allMovements = before ++ newMovements ++ after
            in
                List.enumerated allMovements

        AddMovement ->
            let
                rawMovements = List.map Tuple.second movements
            in
                List.enumerated <| rawMovements ++ [""]

movementsAround : Int -> Movements -> (List Movement, List Movement)
movementsAround idx movements
    = movements
    |> List.map Tuple.second
    |> List.splitAt idx
    |> Tuple.mapSecond (List.drop 1)

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
        , value <| movement
        , onInput (\m -> MovementChanged (idx, m))
        ]
        []

splitMovements : Movement -> Movements
splitMovements str
    = str
    |> String.split "+"
    |> List.map String.trim

main : Program Never Movements Msg
main = beginnerProgram
    { model = []
    , view = view
    , update = update
    }
