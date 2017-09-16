module Main exposing (main)

import Html exposing (..)

type Msg
    = None

type alias Model = String

initialModel : Model
initialModel = "foo"

view : Model -> Html Msg
view model = text model

update : Msg -> Model -> Model
update msg model = case msg of
    None -> model

main : Program Never Model Msg
main = Html.beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    }
