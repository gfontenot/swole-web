module Swole.Types.RepScheme exposing (RepScheme, new, setAt)

import Helpers.Maybe as Maybe
import List.Extra as List
import Swole.Types.Rep as Rep exposing (Rep)


type alias RepScheme =
    List Rep


setAt : Int -> Rep -> RepScheme -> RepScheme
setAt idx rep scheme =
    scheme
        |> List.setAt idx rep
        |> Maybe.fromJust


new : Int -> RepScheme
new count =
    List.repeat count (Rep.fromInt 0)
