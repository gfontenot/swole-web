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
    ( Weight
    , WeightUnit(..)
    , toWeightUnit
    )

type Msg
    = AmountChanged Int
    | UnitChanged WeightUnit

view : Weight -> Html Msg
view weight =
    div []
        [ amountField weight.amount
        , unitPicker weight.unit
        ]

update : Msg -> Weight -> Weight
update msg weight = case msg of
    AmountChanged amount ->
        { weight | amount = amount }

    UnitChanged unit ->
        { weight | unit = unit }

amountField : Int -> Html Msg
amountField v =
    input
        [ type_ "text"
        , placeholder "weight"
        , value <| toString v
        , onInput (AmountChanged << parseAmount)
        ]
        []

unitPicker : WeightUnit -> Html Msg
unitPicker unit =
    select
        [ onChange (UnitChanged << parseUnit) ]
        [ unitOption unit Pounds
        , unitOption unit Kilos
        ]

unitOption : WeightUnit -> WeightUnit -> Html Msg
unitOption current unit =
    option
        [ value <| toString unit
        , selected <| current == unit
        ]
        [ text <| toString unit ]

parseAmount : String -> Int
parseAmount str
    = String.toInt str
    |> Result.withDefault 0

parseUnit : String -> WeightUnit
parseUnit str
    = toWeightUnit str
    |> Result.withDefault Kilos
