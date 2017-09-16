module Swole.Components.SingleSet exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Swole.Types exposing (..)
import Helpers.Events exposing (onChange)

type Msg
    = RepsUpdated String
    | WeightUpdated Int
    | WeightUnitUpdated WeightUnit

initialModel : WorkoutSet
initialModel =
    { reps = []
    , weight =
        { amount = 0
        , unit = Kilos
        }
    }

view : WorkoutSet -> Html Msg
view set =
    div []
        [ input [type_ "text", placeholder "reps", onInput RepsUpdated ] []
        , input [type_ "text", placeholder "weight", onInput (WeightUpdated << parseInt)] []
        , select [ onChange (WeightUnitUpdated << parseUnit) ]
            [ viewUnit set.weight Pounds
            , viewUnit set.weight Kilos
            ]
        , text <| setToString set
        ]

viewUnit : Weight -> WeightUnit -> Html Msg
viewUnit current unit = option
    [ value <| toString unit
    , selected <| current.unit == unit
    ]
    [ text <| toString unit ]

update : Msg -> WorkoutSet -> WorkoutSet
update msg set = case msg of
    RepsUpdated str ->
        { set | reps = parseReps str }
    WeightUpdated n ->
        updateWeightAmount set n
    WeightUnitUpdated unit ->
        updateWeightUnit set unit

parseReps : String -> List Int
parseReps str
    =  String.split "+" str
    |> List.map String.trim
    |> List.map parseInt

parseInt : String -> Int
parseInt s
    = String.toInt s
    |> Result.withDefault 0

parseUnit : String -> WeightUnit
parseUnit str
    = toWeightUnit str
    |> Result.withDefault Kilos

main : Program Never WorkoutSet Msg
main = Html.beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    }
