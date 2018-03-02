module Swole.Types.Complex exposing
    ( Complex
    , new
    , fromMovement
    , indexedMap
    , insertAt
    , movementCount
    , removeAt
    )

import List.Extra as List

import Swole.Types.Movement exposing (Movement)

type alias Complex = List Movement

new : Complex
new = []

fromMovement : Movement -> Complex
fromMovement = String.split "+"

indexedMap : (Int -> Movement -> a) -> Complex -> List a
indexedMap = List.indexedMap

insertAt : Int -> Complex -> Complex -> Complex
insertAt idx new original =
    let
        (before, after) = splitAround idx original
    in
        before ++ new ++ after

movementCount : Complex -> Int
movementCount = List.length

removeAt : Int -> Complex -> Complex
removeAt = List.removeAt

splitAround : Int -> Complex -> (Complex, Complex)
splitAround idx complex
    = complex
    |> List.splitAt idx
    |> Tuple.mapSecond (List.drop 1)
