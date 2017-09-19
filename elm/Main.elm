module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (onClick)
import List.Extra as List

import Swole.Components.SingleSet as SingleSet
import Helpers.Maybe as Maybe
import Helpers.List as List

type Msg
    = SingleSetMsg (Int, SingleSet.Msg)
    | AddSet
    | DeleteSet Int

type alias Model =
    { setModels : List SingleSet.Model
    }

initialModel : Model
initialModel =
    { setModels = [SingleSet.initialModel]
    }

view : Model -> Html Msg
view model =
    let
        sets = List.enumerated model.setModels
    in
        div []
            [ ol [] (List.map viewSet sets)
            , button [ onClick AddSet ] [ text "Add set" ]
            ]

viewSet : (Int, SingleSet.Model) -> Html Msg
viewSet (i, setModel) =
    li []
        [ map (SingleSetMsg << \m -> (i, m)) (SingleSet.view setModel)
        , button [onClick (DeleteSet i)] [ text "Delete" ]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddSet ->
            { model | setModels = model.setModels ++ [SingleSet.initialModel] }
        DeleteSet i ->
            { model | setModels = List.removeAt i model.setModels }

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
               { model | setModels = models }

main : Program Never Model Msg
main = beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    }
