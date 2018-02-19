module Swole.Components.MovementInput exposing
    ( main )

import Html exposing (Html, beginnerProgram, div, input, text, button)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)

type alias Movement = String
type alias Movements = List Movement

type Msg
    = MovementChanged Movement
    | AddMovement

view : Movements -> Html Msg
view movements =
    div []
        (movementFields movements)

update : Msg -> Movements -> Movements
update msg movements =
    case msg of
        MovementChanged m ->
            splitMovements m

        AddMovement ->
            movements ++ [""]

movementFields : Movements -> List (Html Msg)
movementFields movements = case movements of
    [] -> [movementField ""]
    ms ->
        let fields = List.intersperse plusLabel (List.map movementField ms)
        in
           fields ++ [plusButton]

plusLabel : Html Msg
plusLabel = text "+"

plusButton : Html Msg
plusButton = button [ onClick AddMovement ] [ text "+" ]

movementField : Movement -> Html Msg
movementField movement =
    input
        [ type_ "text"
        , placeholder "movement"
        , value <| movement
        , onInput MovementChanged
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
