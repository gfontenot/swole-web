module Swole.Components.SingleSet exposing
    ( Msg
    , Model
    , view
    , update
    , initialModel
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Swole.Types exposing (..)
import Helpers.Events exposing (onChange)

type Msg
    = RepsUpdated String
    | WeightUpdated Int
    | WeightUnitUpdated WeightUnit

type alias Model = WorkoutSet

initialModel : Model
initialModel =
    { reps = []
    , weight =
        { amount = 0
        , unit = Kilos
        }
    }

view : Model -> Html Msg
view set =
    div []
        [ repsField set.reps
        , weightField set.weight
        , weightUnitPicker set.weight
        , text <| setToString set
        ]

repsField : List Int -> Html Msg
repsField reps =
    input
        [ type_ "text"
        , placeholder "reps"
        , value (formatReps reps)
        , onInput RepsUpdated
        ]
        []

weightField : Weight -> Html Msg
weightField weight =
    input
        [ type_ "text"
        , placeholder "weight"
        , value (toString weight.amount)
        , onInput (WeightUpdated << parseInt)
        ]
        []

weightUnitPicker : Weight -> Html Msg
weightUnitPicker weight =
    select
        [ onChange (WeightUnitUpdated << parseUnit) ]
        [ unitOption weight Pounds
        , unitOption weight Kilos
        ]

unitOption : Weight -> WeightUnit -> Html Msg
unitOption current unit =
    option
        [ value <| toString unit
        , selected <| current.unit == unit
        ]
        [ text <| toString unit ]

update : Msg -> Model -> Model
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
