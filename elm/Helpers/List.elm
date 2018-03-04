module Helpers.List exposing (enumerated)

import List.Extra exposing (zip)


enumerated : List a -> List ( Int, a )
enumerated xs =
    let
        length =
            List.length xs

        range =
            List.range 0 length
    in
    zip range xs
