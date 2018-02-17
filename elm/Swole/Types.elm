module Swole.Types exposing
    ( WorkoutSet
    , formatReps
    , parseScheme
    , schemeLength
    , setToString
    , updateWeightAmount
    , updateWeightUnit
    , weightToString
    , weightUnitToString
    )

import Swole.Types.Weight exposing (Weight, WeightUnit(..))

type alias WorkoutSet =
    { reps : List Int
    , weight : Weight
    }

parseScheme : String -> List String
parseScheme str
    = String.split "+" str
    |> List.map String.trim

schemeLength : String -> Int
schemeLength str
    = parseScheme str
    |> List.length

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
