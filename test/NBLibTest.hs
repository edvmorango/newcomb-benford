module NBLibTest where

import NBLib
import Test.Hspec

itemA = Item "a" 1

itemA2 = Item "a" 2

itemGroupA :: [NBItemGroup]
itemGroupA =
  ItemGroup 1 2 [itemA, itemA] : fmap (\i -> ItemGroup i 0 []) [2 .. 9]

itemGroupA12 :: [NBItemGroup]
itemGroupA12 =
  [ItemGroup 1 2 [itemA, itemA], ItemGroup 2 1 [itemA2]] ++
  (fmap (\i -> ItemGroup i 0 []) [3 .. 9])

batchA = Batch "a" 2 itemGroupA

batchA12 = Batch "a" 3 itemGroupA12

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
        it "Should return a Batch group A1" $ do
          (mkBatchGroups 1 [(1, itemA), (1, itemA)]) `shouldBe` itemGroupA
        it "Should return a Batch group A1,A2" $ do
          (mkBatchGroups 1 [(1, itemA), (1, itemA), (2, itemA2)]) `shouldBe`
            itemGroupA12
      describe "prepareGroups" $ do
        it "Should return the respective groups A1" $ do
          prepareGroups [itemA, itemA] `shouldBe` itemGroupA
        it "Should return the respective groups A1,A2" $ do
          prepareGroups [itemA, itemA, itemA2] `shouldBe` itemGroupA12
      describe "mkNBBatch" $ do
        it "Should create a new NBBatch" $ do
          mkNBBatch "a" [itemA, itemA] `shouldBe` batchA
        it "Should create a new NBBatch A1, A12" $ do
          mkNBBatch "a" [itemA, itemA, itemA2] `shouldBe` batchA12
