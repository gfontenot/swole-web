module App exposing (main)

import Html
import State
import Types
import View


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { init = State.initialState
        , update = State.update
        , subscriptions = always Sub.none
        , view = View.rootView
        }
