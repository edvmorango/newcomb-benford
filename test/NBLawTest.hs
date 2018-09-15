{-# LANGUAGE ImplicitParams, Rank2Types #-}

module NBLawTest where

import NBLaw
import Test.HUnit.Approx
import Test.Hspec

test :: IO ()
test =
  let ?epsilon = 0.05
   in hspec $ do
        describe "Newcomb Benford's Law" $ do
          it "probability of 1 shouldBe ~0.3010" $ do
            (@~?) 0.3010 (algarismLaw 1)
          it "probability of 2 shouldBe ~0.1760" $ do
            (@~?) 0.1760 (algarismLaw 2)
          it "probability of 3 shouldBe ~0.1249" $ do
            (@~?) 0.1249 (algarismLaw 3)
          it "probability of 4 shouldBe ~0.0969" $ do
            (@~?) 0.0969 (algarismLaw 4)
          it "probability of 5 shouldBe ~0.0791" $ do
            (@~?) 0.0791 (algarismLaw 5)
          it "probability of 6 shouldBe ~0.0669" $ do
            (@~?) 0.0669 (algarismLaw 6)
          it "probability of 7 shouldBe ~0.0579" $ do
            (@~?) 0.0579 (algarismLaw 7)
          it "probability of 8 shouldBe ~0.0511" $ do
            (@~?) 0.0511 (algarismLaw 8)
          it "probability of 9 shouldBe ~0.0462" $ do
            (@~?) 0.0462 (algarismLaw 9)
