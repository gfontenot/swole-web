module Swole.Types.Weight exposing
    ( Weight(..)
    , WeightUnit
    , availableWeightUnits
    , hasUnit
    , setAmount
    , setUnit
    , toWeightConstructor
    , weightAmount
    )

type alias WeightUnit = Int -> Weight

type Weight
    = Pounds Int
    | Kilos Int

toWeightConstructor : String -> Result String WeightUnit
toWeightConstructor str = case str of
    "Pounds" -> Ok Pounds
    "Kilos" -> Ok Kilos
    _ -> Err ("Not a valid weight unit: " ++ str)

unitToString : Weight -> String
unitToString weight = case weight of
    Kilos _ -> "Kilos"
    Pounds _ -> "Pounds"

availableWeightUnits : List String
availableWeightUnits = List.map unitToString [Pounds 0, Kilos 0]

setAmount : Weight -> Int -> Weight
setAmount weight amount = case weight of
    Kilos _ -> Kilos amount
    Pounds _ -> Pounds amount

setUnit : Weight -> WeightUnit -> Weight
setUnit weight newUnit = case weight of
    Kilos amount -> newUnit amount
    Pounds amount -> newUnit amount

weightAmount : Weight -> Int
weightAmount weight = case weight of
    Kilos n -> n
    Pounds n -> n

hasUnit : Weight -> String -> Bool
hasUnit weight unit = unitToString weight == unit
