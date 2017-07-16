module Calculator.Weight
    ( weightInKilos
    , weightInPounds
    ) where

import Import

kiloToPoundRatio :: Double
kiloToPoundRatio = 2.20462

weightInKilos :: Weight -> Double
weightInKilos (Kilos amt) = amt
weightInKilos (Pounds amt) = amt / kiloToPoundRatio

weightInPounds :: Weight -> Double
weightInPounds (Pounds amt) = amt
weightInPounds (Kilos amt) = amt * kiloToPoundRatio
