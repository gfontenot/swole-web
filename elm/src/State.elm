module State exposing (initialState, update)

import Types exposing (Model, Msg)


initialState : ( Model, Cmd Msg )
initialState =
    ( "Hello World", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
