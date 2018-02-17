module Swole.Types exposing
    ( schemeLength
    )

parseScheme : String -> List String
parseScheme str
    = String.split "+" str
    |> List.map String.trim

schemeLength : String -> Int
schemeLength str
    = parseScheme str
    |> List.length
