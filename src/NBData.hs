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
  | P0
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

data Batch = Batch
  { btag :: Tag
  , groups :: [ItemGroup]
  , btotal :: Integer
  } deriving (Eq, Show)

instance Semigroup ItemGroup where
  EmptyGroup <> g = g
  g <> EmptyGroup = g
  (ItemGroup p es t) <> (ItemGroup _ es' t') = ItemGroup p (es <> es') (t + t')

instance Monoid ItemGroup where
  mempty = EmptyGroup
  mappend i i' = i <> i'
