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
          it "1 shouldBe ~0.3010" $ do (@~?) 0.3010 (algarismLaw 1)
