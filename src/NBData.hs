module NBData where

import Data.Monoid
import Data.Semigroup

type Tag = String

data Prefix
  = P1
  | P2
  | P3
  | P4
  | P5
  | P6
  | P7
  | P8
  | P9
  deriving (Eq, Show)

data Item = Item
  { tag :: Tag
  , code :: Integer
  } deriving (Eq, Show)

data ItemGroup
  = EmptyGroup
  | ItemGroup Prefix
              [Item]
              Integer
  deriving (Eq, Show)

-- Tag Groups ElementsTotal
data Batch = Batch
  { groups :: [ItemGroup]
  , total :: Integer
  } deriving (Eq, Show)

data BatchC = BatchC
  { btag :: Tag
  , batch :: Batch
  } deriving (Eq, Show)

instance Semigroup ItemGroup where
  EmptyGroup <> g = g
  g <> EmptyGroup = g
  (ItemGroup p es t) <> (ItemGroup _ es' t') = ItemGroup p (es <> es') (t + t')

instance Monoid ItemGroup where
  mempty = EmptyGroup
  mappend = (<>)

mergeItemGroups :: [ItemGroup] -> [ItemGroup] -> [ItemGroup]
mergeItemGroups a b = map (\(a, b) -> a <> b) (zip a b)

instance Semigroup Batch where
  (Batch g t) <> (Batch g' t') = Batch (mergeItemGroups g g') (t + t')

instance Monoid Batch where
  mempty = Batch [] 0
  mappend = (<>)
