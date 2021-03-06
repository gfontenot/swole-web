module Swole.Types.WorkoutSet exposing (WorkoutSet, new)

import Swole.Types.RepScheme as RepScheme exposing (RepScheme)
import Swole.Types.Weight exposing (Weight)


type alias WorkoutSet =
    { repScheme : RepScheme
    , weight : Weight
    }


new : Int -> Weight -> WorkoutSet
new count weight =
    { repScheme = RepScheme.new count
    , weight = weight
    }
