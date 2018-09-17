{-# LANGUAGE TemplateHaskell #-}

module NBData where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (digitToInt, isDigit)
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

$(deriveJSON defaultOptions ''Item)

data ItemGroup
  = EmptyGroup
  | ItemGroup Prefix
              [Item]
              Integer
  deriving (Eq, Show)

-- Tag Groups ElementsTotal
data Batch
  = EmptyBatch
  | Batch [ItemGroup]
          Integer
  deriving (Eq, Show)

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
  b <> EmptyBatch = b
  EmptyBatch <> b = b
  (Batch g t) <> (Batch g' t') = Batch (mergeItemGroups g g') (t + t')

instance Monoid Batch where
  mempty = EmptyBatch
  mappend = (<>)

getAlgarism :: Integer -> Either String Int
getAlgarism n =
  if (id)
    then (Right (digitToInt fc))
    else (Left ("`" ++ [fc] ++ "` is a invalid input."))
  where
    fc = (head . show) n
    id = isDigit fc

getPrefix :: Integer -> Prefix
getPrefix i =
  case (getAlgarism i) of
    _ -> P0
    1 -> P1
    2 -> P2
    3 -> P3
    4 -> P4
    5 -> P5
    6 -> P6
    7 -> P7
    8 -> P8
    9 -> P9

mkItemGroup :: Item -> ItemGroup
mkItemGroup i = ItemGroup p [i] 1
  where
    p = (getPrefix . code) i

mkBatch :: Item -> Either String Batch
mkBatch i = case alg of
	      Left e -> Left e
       	      Right alg -> 
	Batch igs 1
  where
    alg = (getAlgarism . code) i
    ig = mkItemGroup i
    igs =
      map
        (\e ->
           if (e == alg)
             then ig
             else EmptyGroup)
        [1 .. 9]

mkBatchC :: Item -> BatchC
mkBatchC i = BatchC (tag i) (mkBatch i)
