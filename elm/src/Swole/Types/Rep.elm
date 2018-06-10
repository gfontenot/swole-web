module Swole.Types.Rep exposing (Rep, fromInt, fromString, toString)


type Rep
    = Rep Int


fromString : String -> Rep
fromString str =
    String.toInt str
        |> Result.withDefault 0
        |> fromInt


toString : Rep -> String
toString (Rep n) =
    Basics.toString n


fromInt : Int -> Rep
fromInt =
    Rep
