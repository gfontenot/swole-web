module Weight.State exposing (init, subscriptions, update)

import Weight.Types exposing (Msg(..), Weight(..), weightAmount, weightUnit)


init : ( Weight, Cmd Msg )
init =
    ( Kilos 0, Cmd.none )


subscriptions : Weight -> Sub Msg
subscriptions =
    always Sub.none


update : Msg -> Weight -> Weight
update msg weight =
    case msg of
        AmountChanged amt ->
            weightAmount.set amt weight

        UnitChanged newUnit ->
            weightUnit.set newUnit weight
