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

import Swole.Types exposing (WeightUnit(..), toWeightUnit)
import Helpers.Events exposing (onChange)

type Msg
    = RepsUpdated String
    | WeightUpdated String
    | WeightUnitUpdated WeightUnit

type alias Model =
    { movementCount: Int
    , reps : String
    , weightAmount : String
    , weightUnit : WeightUnit
    }

initialModel : Int -> Model
initialModel count =
    { movementCount = count
    , reps = ""
    , weightAmount = ""
    , weightUnit = Kilos
    }

view : Model -> Html Msg
view model =
    div []
        [ repsField model.reps
        , weightField model.weightAmount
        , weightUnitPicker model.weightUnit
        ]

repsField : String -> Html Msg
repsField v =
    input
        [ type_ "text"
        , placeholder "reps"
        , value v
        , onInput RepsUpdated
        ]
        []

weightField : String -> Html Msg
weightField v =
    input
        [ type_ "text"
        , placeholder "weight"
        , value v
        , onInput WeightUpdated
        ]
        []

weightUnitPicker : WeightUnit -> Html Msg
weightUnitPicker unit =
    select
        [ onChange (WeightUnitUpdated << parseUnit) ]
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

update : Msg -> Model -> Model
update msg model = case msg of
    RepsUpdated str ->
        { model | reps = str }
    WeightUpdated str ->
        { model | weightAmount = str }
    WeightUnitUpdated unit ->
        { model | weightUnit = unit }

parseUnit : String -> WeightUnit
parseUnit str
    = toWeightUnit str
    |> Result.withDefault Kilos
