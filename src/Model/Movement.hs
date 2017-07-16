module Model.Movement
    ( movementWeight
    ) where

import Model.Persist
import Model.Weight

movementWeight :: Movement -> Weight
movementWeight (Movement _ _ amt KiloUnit _) = Kilos amt
movementWeight (Movement _ _ amt PoundUnit _) = Pounds amt
