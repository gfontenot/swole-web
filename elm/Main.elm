module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as List

import Swole.Components.MovementInput as MovementInput
import Swole.Components.SingleSet as SingleSet
import Helpers.Maybe as Maybe
import Helpers.List as List

type Msg
    = MovementInputMsg MovementInput.Msg
    | SingleSetMsg (Int, SingleSet.Msg)
    | AddSet
    | DeleteSet Int

type alias Model =
    { movements : MovementInput.Model
    , setModels : List SingleSet.Model
    }

initialModel : Model
initialModel =
    { movements = []
    , setModels = [SingleSet.initialModel 0]
    }

view : Model -> Html Msg
view model =
    let
        sets = List.enumerated model.setModels
    in
        div []
            [ map MovementInputMsg <| MovementInput.view model.movements
            , ol [] (List.map viewSet sets)
            , button [ onClick AddSet ] [ text "Add set" ]
            ]

viewSet : (Int, SingleSet.Model) -> Html Msg
viewSet (i, setModel) =
    li []
        [ map (SingleSetMsg << \m -> (i, m)) (SingleSet.view setModel)
        , button [onClick (DeleteSet i)] [ text "Delete" ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SingleSetMsg (i, m) ->
            let
                updated
                    = List.getAt i model.setModels
                    |> Maybe.fromJust
                    |> SingleSet.update m

                models
                    = List.setAt i updated model.setModels
                    |> Maybe.fromJust

            in
               ({ model | setModels = models }, Cmd.none)

        MovementInputMsg msg ->
            let
                (updated, cmd) = MovementInput.update msg model.movements
            in
                ({ model | movements = updated }, Cmd.map MovementInputMsg cmd)

        AddSet ->
            let
                count = 0
                newSets = model.setModels ++ [SingleSet.initialModel count]
            in
                ({ model | setModels = newSets }, Cmd.none)

        DeleteSet i ->
            let
                newSets = List.removeAt i model.setModels
            in
               ({ model | setModels = newSets }, Cmd.none)


main : Program Never Model Msg
main = program
    { init = (initialModel, Cmd.none)
    , update = update
    , subscriptions = (always Sub.none)
    , view = view
    }
