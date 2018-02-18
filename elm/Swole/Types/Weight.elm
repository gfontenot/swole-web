module Swole.Types.Weight exposing
    ( Weight(..)
    , availableWeightUnits
    , hasUnit
    , setAmount
    , setUnit
    , toWeightConstructor
    , weightAmount
    )

type Weight
    = Pounds Int
    | Kilos Int

toWeightConstructor : String -> Result String (Int -> Weight)
toWeightConstructor str = case str of
    "Pounds" -> Ok Pounds
    "Kilos" -> Ok Kilos
    _ -> Err ("Not a valid weight unit: " ++ str)

availableWeightUnits : List String
availableWeightUnits =
    [ "Pounds"
    , "Kilos"
    ]

setAmount : Weight -> Int -> Weight
setAmount weight amount = case weight of
    Kilos _ -> Kilos amount
    Pounds _ -> Pounds amount

setUnit : Weight -> (Int -> Weight) -> Weight
setUnit weight newUnit = case weight of
    Kilos amount -> newUnit amount
    Pounds amount -> newUnit amount

weightAmount : Weight -> Int
weightAmount weight = case weight of
    Kilos n -> n
    Pounds n -> n

unitToString : Weight -> String
unitToString weight = case weight of
    Kilos _ -> "Kilos"
    Pounds _ -> "Pounds"

hasUnit : Weight -> String -> Bool
hasUnit weight unit = unitToString weight == unit
