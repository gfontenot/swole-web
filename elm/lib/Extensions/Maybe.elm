module Extensions.Maybe exposing (fromJust)


fromJust : Maybe a -> a
fromJust ma =
    case ma of
        Just a ->
            a

        Nothing ->
            Debug.crash "Unexpectedly found Nothing"
