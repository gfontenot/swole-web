module Swole.Types exposing
    ( WeightUnit (..)
    , Weight
    , WorkoutSet
    , toWeightUnit
    , weightToString
    , weightUnitToString
    , setToString
    , updateWeightUnit
    , updateWeightAmount
    )

type WeightUnit
    = Pounds
    | Kilos

type alias Weight =
    { amount : Int
    , unit : WeightUnit
    }

type alias WorkoutSet =
    { reps : List Int
    , weight : Weight
    }

toWeightUnit : String -> Result String WeightUnit
toWeightUnit str = case str of
    "Pounds" -> Ok Pounds
    "Kilos" -> Ok Kilos
    _ -> Err ("Not a valid weight unit: " ++ str)

weightToString : Weight -> String
weightToString weight =
    toString weight.amount ++ " " ++ (weightUnitToString weight.unit)

weightUnitToString : WeightUnit -> String
weightUnitToString unit = case unit of
    Pounds -> "lbs"
    Kilos -> "kgs"

setToString : WorkoutSet -> String
setToString set =
    let
        reps = String.join " + " (List.map toString set.reps)
    in
       reps ++ " @ " ++ weightToString set.weight

updateWeightUnit : WorkoutSet -> WeightUnit -> WorkoutSet
updateWeightUnit set unit =
    let
        current = set.weight
    in
       { set | weight = { current | unit = unit } }

updateWeightAmount : WorkoutSet -> Int -> WorkoutSet
updateWeightAmount set amount =
    let
        current = set.weight
    in
       { set | weight = { current | amount = amount } }
