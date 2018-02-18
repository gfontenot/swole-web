module Swole.Components.Weight exposing
    ( Msg(..)
    , update
    , view
    )

import Html exposing
    ( Html
    , div
    , input
    , option
    , select
    , text
    )

import Html.Attributes exposing (placeholder, selected, type_, value)
import Html.Events exposing (onInput)
import Helpers.Events exposing (onChange)

import Swole.Types.Weight exposing
    ( Weight(..)
    , WeightUnit
    , availableWeightUnits
    , hasUnit
    , setAmount
    , setUnit
    , toWeightConstructor
    , weightAmount
    )

type Msg
    = AmountChanged Int
    | UnitChanged WeightUnit

view : Weight -> Html Msg
view weight =
    div []
        [ amountField <| weightAmount weight
        , unitPicker weight
        ]

update : Msg -> Weight -> Weight
update msg weight = case msg of
    AmountChanged amount ->
        setAmount weight amount

    UnitChanged newUnit ->
        setUnit weight newUnit

amountField : Int -> Html Msg
amountField v =
    input
        [ type_ "text"
        , placeholder "weight"
        , value <| toString v
        , onInput (AmountChanged << parseAmount)
        ]
        []

unitPicker : Weight -> Html Msg
unitPicker weight =
    select
        [ onChange (UnitChanged << parseUnit) ]
        (List.map (unitOption weight) availableWeightUnits)

unitOption : Weight -> String -> Html Msg
unitOption weight unit =
    option
        [ value unit
        , selected <| hasUnit weight unit
        ]
        [ text unit ]

parseAmount : String -> Int
parseAmount str
    = String.toInt str
    |> Result.withDefault 0

parseUnit : String -> WeightUnit
parseUnit str
    = toWeightConstructor str
    |> Result.withDefault Kilos
