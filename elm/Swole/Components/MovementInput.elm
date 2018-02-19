module Swole.Components.MovementInput exposing
    ( main )

import Html exposing (Html, beginnerProgram, div, input)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)

type alias Movement = String

type alias Model = List Movement

type Msg
    = MovementChanged Movement

view : Model -> Html Msg
view model =
    div []
        (movementFields model)

update : Msg -> Model -> Model
update msg model =
    case msg of
        MovementChanged m -> splitMovements m

movementFields : List Movement -> List (Html Msg)
movementFields model = case model of
    [] -> [movementField ""]
    ms -> List.map movementField ms

movementField : Movement -> Html Msg
movementField movement =
    input
        [ type_ "text"
        , placeholder "movement"
        , value <| movement
        , onInput MovementChanged
        ]
        []

splitMovements : Movement -> List Movement
splitMovements str
    = str
    |> String.split "+"
    |> List.map String.trim

main : Program Never Model Msg
main = beginnerProgram
    { model = []
    , view = view
    , update = update
    }
