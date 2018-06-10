module Swole.Types.Weight
    exposing
        ( Weight(..)
        , WeightUnit
        , availableUnits
        , hasUnit
        , toUnit
        , weightAmount
        , weightUnit
        )

import Lens exposing (Lens)


type alias WeightUnit =
    Int -> Weight


type Weight
    = Pounds Int
    | Kilos Int


toUnit : String -> Result String WeightUnit
toUnit str =
    case str of
        "Pounds" ->
            Ok Pounds

        "Kilos" ->
            Ok Kilos

        _ ->
            Err ("Not a valid weight unit: " ++ str)


unitToString : Weight -> String
unitToString weight =
    case weight of
        Kilos _ ->
            "Kilos"

        Pounds _ ->
            "Pounds"


availableUnits : List String
availableUnits =
    List.map unitToString [ Pounds 0, Kilos 0 ]


weightUnit : Lens Weight WeightUnit
weightUnit =
    let
        get weight =
            case weight of
                Kilos _ ->
                    Kilos

                Pounds _ ->
                    Pounds

        set unit weight =
            case weight of
                Kilos amt ->
                    unit amt

                Pounds amt ->
                    unit amt
    in
    Lens get set


weightAmount : Lens Weight Int
weightAmount =
    let
        get weight =
            case weight of
                Kilos amt ->
                    amt

                Pounds amt ->
                    amt

        set amt weight =
            case weight of
                Kilos _ ->
                    Kilos amt

                Pounds _ ->
                    Pounds amt
    in
    Lens get set


hasUnit : Weight -> String -> Bool
hasUnit weight unit =
    unitToString weight == unit
