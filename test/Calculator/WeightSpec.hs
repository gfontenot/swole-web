module Calculator.WeightSpec (spec) where

import TestImport
import Calculator.Weight

spec :: Spec
spec = do
    describe "weightInKilos" $ do
        it "converts pounds into kilos" $ do
            weightInKilos (Pounds 45)  `shouldBe` 20.41168092460379
            weightInKilos (Pounds 225) `shouldBe` 102.05840462301894
            weightInKilos (Pounds 315) `shouldBe` 142.88176647222653

        it "doesn't convert kilos" $ do
            weightInKilos (Kilos 20)  `shouldBe` 20
            weightInKilos (Kilos 100) `shouldBe` 100
            weightInKilos (Kilos 150) `shouldBe` 150


    describe "weightInPounds" $ do
        it "converts kilos to pounds" $ do
            weightInPounds (Kilos 60)  `shouldBe` 132.2772
            weightInPounds (Kilos 100) `shouldBe` 220.462
            weightInPounds (Kilos 150) `shouldBe` 330.693

        it "doesn't convert pounds" $ do
            weightInPounds (Pounds 135) `shouldBe` 135
            weightInPounds (Pounds 225) `shouldBe` 225
            weightInPounds (Pounds 315) `shouldBe` 315
