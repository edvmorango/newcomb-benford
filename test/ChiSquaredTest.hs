{-# LANGUAGE ImplicitParams, Rank2Types #-}

module ChiSquaredTest where

import ChiSquared
import Test.HUnit.Approx
import Test.Hspec

listEfRf :: [(Float, Float)]
listEfRf =
  [ (301, 302)
  , (176, 175)
  , (125, 126)
  , (97, 96)
  , (79, 80)
  , (67, 66)
  , (58, 59)
  , (51, 50)
  , (46, 46)
  ]

expectedCS :: [Float]
expectedCS =
  [ 0.003322259
  , 0.0056818184
  , 0.008
  , 0.010309278
  , 0.012658228
  , 0.014925373
  , 0.01724138
  , 0.019607844
  , 0.0
  ]

tests :: IO ()
tests =
  let ?epsilon = 0.05
   in hspec $ do
        describe "ChiSquared Formula Test" $ do
          it "EF = 301, RF = 302 ChiSquared shouldBe ~ 0.003" $ do
            (@~?) 0.003 (chiSquaredFormula 301 302)
          it "EF = 176, RF = 175 ChiSquared shouldBe ~ 0.003" $ do
            (@~?) 0.006 (chiSquaredFormula 176 175)
          it "EF = 125, RF = 126 ChiSquared shouldBe ~ 0.008" $ do
            (@~?) 0.008 (chiSquaredFormula 125 126)
          it "EF = 97, RF = 96 ChiSquared shouldBe ~ 0.010" $ do
            (@~?) 0.010 (chiSquaredFormula 97 96)
          it "EF = 79, RF = 80 ChiSquared shouldBe ~ 0.013" $ do
            (@~?) 0.013 (chiSquaredFormula 79 80)
          it "EF = 67, RF = 66 ChiSquared shouldBe ~ 0.015" $ do
            (@~?) 0.015 (chiSquaredFormula 67 66)
          it "EF = 58, RF = 59 ChiSquared shouldBe ~ 0.017" $ do
            (@~?) 0.017 (chiSquaredFormula 58 59)
          it "EF = 51, RF = 50 ChiSquared shouldBe ~ 0.010" $ do
            (@~?) 0.010 (chiSquaredFormula 51 50)
          it "EF = 46, RF = 46 ChiSquared shouldBe ~ 0.000" $ do
            (@~?) 0.000 (chiSquaredFormula 46 46)
        describe "ChiSquared Executor Test" $ do
          it "listEfRf ChiSquaredExecutor shouldBe expectedCS" $ do
            (chiSquaredExecutor listEfRf) `shouldBe` expectedCS
        describe "ChiSquared frequency Test" $ do
          it "listEfRf frequency total of Elements shouldBe 1000.0" $ do
            (fst (frequency listEfRf)) `shouldBe` 1000.0
          it "listEfRf frequency deviation shouldBe ~0.9" $ do
            (@~?) 0.9 (snd (frequency listEfRf))
