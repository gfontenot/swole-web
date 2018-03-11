module Main exposing (main)

import Html exposing (Html, program)
import Swole.Components.Group as Group
import Swole.Types.Group as Group exposing (Group)


type alias Model =
    Group


type Msg
    = ModelMsg Group.Msg


view : Model -> Html Msg
view model =
    Html.map ModelMsg <| Group.view model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelMsg m ->
            let
                ( updated, cmd ) =
                    Group.update m model
            in
            ( updated, Cmd.map ModelMsg cmd )


main : Program Never Model Msg
main =
    program
        { init = ( Group.new, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
