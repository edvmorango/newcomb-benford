module NBData where

type Tag = String

data Item = Item
  { tag :: Tag
  , code :: Integer
  }

data ItemGroup = ItemGroup
  { prefix :: Int
  , items :: [Item]
  , total :: Integer
  }

data Batch = Batch
  { tag :: Tag
  , groups :: [ItemBatch]
  , total :: Integer
  }
