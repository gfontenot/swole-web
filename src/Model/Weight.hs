module Model.Weight
    ( Weight(..)
    , WeightUnit(..)
    ) where

import ClassyPrelude.Yesod

data Weight = Kilos Double | Pounds Double

data WeightUnit = KiloUnit | PoundUnit
    deriving (Show, Eq, Read)
derivePersistField "WeightUnit"
