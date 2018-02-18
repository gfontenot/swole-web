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

import Swole.Types.Weight as Weight exposing (Weight(..), WeightUnit)

type Msg
    = AmountChanged Int
    | UnitChanged WeightUnit

view : Weight -> Html Msg
view weight =
    div []
        [ amountField <| Weight.amount weight
        , unitPicker weight
        ]

update : Msg -> Weight -> Weight
update msg weight = case msg of
    AmountChanged amount ->
        Weight.setAmount weight amount

    UnitChanged newUnit ->
        Weight.setUnit weight newUnit

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
        (List.map (unitOption weight) Weight.availableUnits)

unitOption : Weight -> String -> Html Msg
unitOption weight unit =
    option
        [ value unit
        , selected <| Weight.hasUnit weight unit
        ]
        [ text unit ]

parseAmount : String -> Int
parseAmount str
    = String.toInt str
    |> Result.withDefault 0

parseUnit : String -> WeightUnit
parseUnit str
    = Weight.toUnit str
    |> Result.withDefault Kilos
