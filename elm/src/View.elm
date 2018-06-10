module View exposing (rootView)

import Html exposing (Html)
import Types exposing (Model, Msg)


rootView : Model -> Html Msg
rootView model =
    Html.text model
