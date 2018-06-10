module Weight.View
    exposing
        ( view
        )

import Extensions.Events exposing (onChange)
import Html
    exposing
        ( Html
        , div
        , input
        , option
        , select
        , text
        )
import Html.Attributes exposing (placeholder, selected, type_, value)
import Html.Events exposing (onInput)
import Weight.Types as Weight
    exposing
        ( Msg(..)
        , Weight(..)
        , WeightUnit
        , weightAmount
        , weightUnit
        )


view : Weight -> Html Msg
view weight =
    div []
        [ amountField <| weightAmount.get weight
        , unitPicker weight
        ]


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
parseAmount str =
    String.toInt str
        |> Result.withDefault 0


parseUnit : String -> WeightUnit
parseUnit str =
    Weight.toUnit str
        |> Result.withDefault Kilos
