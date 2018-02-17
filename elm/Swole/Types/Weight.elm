module Swole.Types.Weight exposing
    ( Weight
    , WeightUnit(..)
    , toWeightUnit
    )

type WeightUnit
    = Pounds
    | Kilos

type alias Weight =
    { amount : Int
    , unit : WeightUnit
    }

toWeightUnit : String -> Result String WeightUnit
toWeightUnit str = case str of
    "Pounds" -> Ok Pounds
    "Kilos" -> Ok Kilos
    _ -> Err ("Not a valid weight unit: " ++ str)
