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

import Swole.Types exposing
    ( parseScheme
    , schemeLength
    )
import Swole.Types.Weight exposing (Weight, WeightUnit(..))
import Swole.Components.Weight as WeightComponent

type Msg
    = RepSchemeUpdated String
    | WeightUpdated WeightComponent.Msg
    | MovementCountUpdated Int

type alias Model =
    { movementCount: Int
    , repScheme : String
    , weight: Weight
    , validRepScheme : Bool
    }

initialModel : Int -> Model
initialModel count =
    { movementCount = count
    , repScheme = ""
    , weight =
        { amount = 0
        , unit = Kilos
        }
    , validRepScheme = True
    }

view : Model -> Html Msg
view model =
    div []
        [ repsField model.repScheme model.validRepScheme
        , Html.map WeightUpdated (WeightComponent.view model.weight)
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

update : Msg -> Model -> Model
update msg model = case msg of
    RepSchemeUpdated str ->
        validate { model | repScheme = str }
    WeightUpdated m ->
        { model | weight = WeightComponent.update m model.weight }
    MovementCountUpdated count ->
        validate { model | movementCount = count }

validate : Model -> Model
validate model =
    let
        repCount = schemeLength model.repScheme
        validRepScheme = repCount == model.movementCount

    in
       { model | validRepScheme = validRepScheme }
