module Swole.Types.Weight exposing
    ( Weight(..)
    , WeightUnit
    , availableUnits
    , hasUnit
    , setAmount
    , setUnit
    , toUnit
    , amount
    )

type alias WeightUnit = Int -> Weight

type Weight
    = Pounds Int
    | Kilos Int

toUnit : String -> Result String WeightUnit
toUnit str = case str of
    "Pounds" -> Ok Pounds
    "Kilos" -> Ok Kilos
    _ -> Err ("Not a valid weight unit: " ++ str)

unitToString : Weight -> String
unitToString weight = case weight of
    Kilos _ -> "Kilos"
    Pounds _ -> "Pounds"

availableUnits : List String
availableUnits = List.map unitToString [Pounds 0, Kilos 0]

setAmount : Weight -> Int -> Weight
setAmount weight amount = case weight of
    Kilos _ -> Kilos amount
    Pounds _ -> Pounds amount

setUnit : Weight -> WeightUnit -> Weight
setUnit weight newUnit = case weight of
    Kilos amount -> newUnit amount
    Pounds amount -> newUnit amount

amount : Weight -> Int
amount weight = case weight of
    Kilos n -> n
    Pounds n -> n

hasUnit : Weight -> String -> Bool
hasUnit weight unit = unitToString weight == unit
