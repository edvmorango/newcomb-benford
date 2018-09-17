module NBDataTest where

import NBData
import Test.Hspec

item1Fixture :: Item
item1Fixture = Item "fixture" 100

item2Fixture :: Item
item2Fixture = Item "fixture" 100

item3Fixture :: Item
item3Fixture = Item "fixture" 300

itemGroup1Fixture :: ItemGroup
itemGroup1Fixture = ItemGroup P1 [item1Fixture] 1

itemGroup2Fixture :: ItemGroup
itemGroup2Fixture = ItemGroup P1 [item2Fixture] 1

itemGroup3Fixture :: ItemGroup
itemGroup3Fixture = ItemGroup P3 [item3Fixture] 1

mergedItemGroup12Fixture :: ItemGroup
mergedItemGroup12Fixture = ItemGroup P1 [item1Fixture, item2Fixture] 2

mergedItemGroupsFixture :: [ItemGroup]
mergedItemGroupsFixture =
  [mergedItemGroup12Fixture, EmptyGroup, itemGroup3Fixture]

nbDataTests :: IO ()
nbDataTests =
  hspec $ do
    describe "NBData tests" $ do
      it "should merge item groups" $ do
        (mergeItemGroups
           [mergedItemGroup12Fixture, EmptyGroup, EmptyGroup]
           [EmptyGroup, EmptyGroup, itemGroup3Fixture]) `shouldBe`
          mergedItemGroupsFixture
