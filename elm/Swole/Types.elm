module Swole.Types exposing
    ( WeightUnit (..)
    , Weight
    , WorkoutSet
    , formatReps
    , parseScheme
    , setToString
    , toWeightUnit
    , updateWeightAmount
    , updateWeightUnit
    , weightToString
    , weightUnitToString
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

parseScheme : String -> List String
parseScheme str
    = String.split "+" str
    |> List.map String.trim

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

formatReps : List Int -> String
formatReps reps = String.join " + " (List.map toString reps)

setToString : WorkoutSet -> String
setToString set =
       formatReps set.reps ++ " @ " ++ weightToString set.weight

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
