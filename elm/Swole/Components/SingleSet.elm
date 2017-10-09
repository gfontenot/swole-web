module Swole.Components.SingleSet exposing
    ( Msg(..)
    , Model
    , view
    , update
    , initialModel
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Swole.Types exposing (WeightUnit(..), toWeightUnit, parseScheme)
import Helpers.Events exposing (onChange)

type Msg
    = RepsUpdated String
    | WeightUpdated String
    | WeightUnitUpdated WeightUnit
    | MovementCountUpdated Int

type alias Model =
    { movementCount: Int
    , reps : String
    , weightAmount : String
    , weightUnit : WeightUnit
    , validRepScheme : Bool
    }

initialModel : Int -> Model
initialModel count =
    { movementCount = count
    , reps = ""
    , weightAmount = ""
    , weightUnit = Kilos
    , validRepScheme = True
    }

view : Model -> Html Msg
view model =
    div []
        [ repsField model.reps model.validRepScheme
        , weightField model.weightAmount
        , weightUnitPicker model.weightUnit
        ]

repsField : String -> Bool -> Html Msg
repsField val valid =
    input
        [ type_ "text"
        , placeholder "reps"
        , value val
        , onInput RepsUpdated
        , validClass valid
        ]
        []

validClass : Bool -> Attribute Msg
validClass valid = case valid of
    True -> class "valid"
    False -> class "invalid"

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
        validate { model | reps = str }
    WeightUpdated str ->
        { model | weightAmount = str }
    WeightUnitUpdated unit ->
        { model | weightUnit = unit }
    MovementCountUpdated count ->
        validate { model | movementCount = count }

validate : Model -> Model
validate model =
    let
        repCount = parseScheme model.reps
            |> List.length

        validRepScheme = repCount == model.movementCount

    in
       { model | validRepScheme = validRepScheme }

parseUnit : String -> WeightUnit
parseUnit str
    = toWeightUnit str
    |> Result.withDefault Kilos
