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

import Helpers.Events exposing (onChange)
import Swole.Types exposing
    ( WeightUnit(..)
    , toWeightUnit
    , parseScheme
    , schemeLength
    )

type Msg
    = RepSchemeUpdated String
    | WeightUpdated String
    | WeightUnitUpdated WeightUnit
    | MovementCountUpdated Int

type alias Model =
    { movementCount: Int
    , repScheme : String
    , weightAmount : String
    , weightUnit : WeightUnit
    , validRepScheme : Bool
    }

initialModel : Int -> Model
initialModel count =
    { movementCount = count
    , repScheme = ""
    , weightAmount = ""
    , weightUnit = Kilos
    , validRepScheme = True
    }

view : Model -> Html Msg
view model =
    div []
        [ repsField model.repScheme model.validRepScheme
        , weightField model.weightAmount
        , weightUnitPicker model.weightUnit
        ]

repsField : String -> Bool -> Html Msg
repsField val valid =
    input
        [ type_ "text"
        , placeholder "reps"
        , value val
        , onInput RepSchemeUpdated
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
    RepSchemeUpdated str ->
        validate { model | repScheme = str }
    WeightUpdated str ->
        { model | weightAmount = str }
    WeightUnitUpdated unit ->
        { model | weightUnit = unit }
    MovementCountUpdated count ->
        validate { model | movementCount = count }

validate : Model -> Model
validate model =
    let
        repCount = schemeLength model.repScheme
        validRepScheme = repCount == model.movementCount

    in
       { model | validRepScheme = validRepScheme }

parseUnit : String -> WeightUnit
parseUnit str
    = toWeightUnit str
    |> Result.withDefault Kilos
