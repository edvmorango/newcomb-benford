module NBLibTest where

import NBLib
import Test.Hspec

itemA = Item "a" 1

itemA2 = Item "a" 2

itemGroupA :: [NBItemGroup]
itemGroupA =
  [ ItemGroup 1 2 [itemA, itemA]
  , ItemGroup 2 0 []
  , ItemGroup 3 0 []
  , ItemGroup 4 0 []
  , ItemGroup 5 0 []
  , ItemGroup 6 0 []
  , ItemGroup 7 0 []
  , ItemGroup 8 0 []
  , ItemGroup 9 0 []
  ]

nbListTest :: IO ()
nbListTest =
  hspec $ do
    describe "Newcomb-Benford's lib tests" $ do
      describe "getPrefix" $ do
        it "Should return Nothing when the number is equals to 0 " $ do
          getPrefix 0 `shouldBe` Nothing
        it "Should return Nothing when the number is lesser than 0 " $ do
          getPrefix (-1) `shouldBe` Nothing
      describe "mkNBItemGroup" $ do
        it "Should create a new ItemGroup " $ do
          (mkNBItemGroup 1 [itemA, itemA]) `shouldBe`
            (ItemGroup 1 2 [itemA, itemA])
        it "Should create a empty ItemGroup" $ do
          (mkNBItemGroup 1 []) `shouldBe` (ItemGroup 1 0 [])
      describe "mkBatchGroups" $ do
        it "Should return a Batch group" $ do
          (mkBatchGroups 1 [(1, itemA), (1, itemA)]) `shouldBe` itemGroupA
