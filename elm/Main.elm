module Main exposing (main)

import Helpers.List as List
import Helpers.Maybe as Maybe
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Swole.Components.Complex as Complex
import Swole.Components.SingleSet as SingleSet
import Swole.Types.Complex as Complex exposing (Complex)


type Msg
    = ComplexChanged Complex.Msg
    | SingleSetMsg ( Int, SingleSet.Msg )
    | AddSet
    | DeleteSet Int


type alias Model =
    { complex : Complex
    , setModels : List SingleSet.Model
    }


initialModel : Model
initialModel =
    { complex = Complex.new
    , setModels = [ SingleSet.initialModel 0 ]
    }


view : Model -> Html Msg
view model =
    let
        sets =
            List.enumerated model.setModels
    in
    div []
        [ map ComplexChanged <| Complex.view model.complex
        , ol [] (List.map viewSet sets)
        , button [ onClick AddSet ] [ text "Add set" ]
        ]


viewSet : ( Int, SingleSet.Model ) -> Html Msg
viewSet ( i, setModel ) =
    li []
        [ map (SingleSetMsg << (\m -> ( i, m ))) (SingleSet.view setModel)
        , button [ onClick (DeleteSet i) ] [ text "Delete" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SingleSetMsg ( i, m ) ->
            let
                updated =
                    List.getAt i model.setModels
                        |> Maybe.fromJust
                        |> SingleSet.update m

                models =
                    List.setAt i updated model.setModels
                        |> Maybe.fromJust
            in
            ( { model | setModels = models }, Cmd.none )

        ComplexChanged msg ->
            let
                ( updated, cmd ) =
                    Complex.update msg model.complex
            in
            ( { model | complex = updated }, Cmd.map ComplexChanged cmd )

        AddSet ->
            let
                count =
                    0

                newSets =
                    model.setModels ++ [ SingleSet.initialModel count ]
            in
            ( { model | setModels = newSets }, Cmd.none )

        DeleteSet i ->
            let
                newSets =
                    List.removeAt i model.setModels
            in
            ( { model | setModels = newSets }, Cmd.none )


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
