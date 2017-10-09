module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List

import Swole.Components.SingleSet as SingleSet
import Helpers.Maybe as Maybe
import Helpers.List as List

type Msg
    = SingleSetMsg (Int, SingleSet.Msg)
    | AddSet
    | DeleteSet Int
    | MovementsUpdated String

type alias Model =
    { movements : List String
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
            [ movementsField model.movements
            , ol [] (List.map viewSet sets)
            , button [ onClick AddSet ] [ text "Add set" ]
            ]

movementsField : List String -> Html Msg
movementsField movements =
    input
        [ type_ "text"
        , placeholder "movements"
        , value <| (String.join " + ") movements
        , onInput MovementsUpdated
        ]
        []

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
            let
                count = List.length model.movements
            in
                { model | setModels = model.setModels ++ [SingleSet.initialModel count] }
        DeleteSet i ->
            { model | setModels = List.removeAt i model.setModels }
        MovementsUpdated ms ->
            let
                movements = parseMovements ms
                count = List.length movements
                setModels = model.setModels
                    |> List.map (SingleSet.update (SingleSet.MovementCountUpdated count))
            in
                { model | movements = movements, setModels = setModels }

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

parseMovements : String -> List String
parseMovements str
    = String.split "+" str
    |> List.map String.trim

main : Program Never Model Msg
main = beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    }
