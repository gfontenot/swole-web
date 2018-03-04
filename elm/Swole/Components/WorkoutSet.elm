module Swole.Components.WorkoutSet
    exposing
        ( Msg
        , update
        , view
        )

import Html exposing (Html, beginnerProgram, div, input, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)
import Swole.Components.Weight as Weight
import Swole.Types.Rep as Rep exposing (Rep)
import Swole.Types.RepScheme as RepScheme exposing (RepScheme)
import Swole.Types.Weight exposing (Weight(..))
import Swole.Types.WorkoutSet exposing (WorkoutSet)


type Msg
    = RepCountChanged Int Rep
    | WeightUpdated Weight.Msg


view : WorkoutSet -> Html Msg
view { repScheme, weight } =
    let
        fields =
            repScheme
                |> List.indexedMap repField
                |> List.intersperse plusLabel
                |> flip List.append [ weightPicker weight ]
    in
    div [] fields


update : Msg -> WorkoutSet -> WorkoutSet
update msg set =
    case msg of
        RepCountChanged idx rep ->
            let
                updated =
                    set.repScheme
                        |> RepScheme.setAt idx rep
            in
            { set | repScheme = updated }

        WeightUpdated m ->
            { set | weight = Weight.update m set.weight }


plusLabel : Html a
plusLabel =
    text "+"


repField : Int -> Rep -> Html Msg
repField idx rep =
    input
        [ type_ "text"
        , placeholder "0"
        , value <| Rep.toString rep
        , onInput <| RepCountChanged idx << Rep.fromString
        ]
        []


weightPicker : Weight -> Html Msg
weightPicker weight =
    weight
        |> Weight.view
        |> Html.map WeightUpdated
