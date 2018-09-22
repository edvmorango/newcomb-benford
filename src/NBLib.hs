{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module NBLib where

import Data.Monoid
import Data.Semigroup

type Tag = String

data Item = Item
  { tag :: String
  , code :: Integer
  } deriving (Eq, Show)

data ItemGroup a b c where
  ItemGroup :: Integer -> Integer -> [Item] -> ItemGroup Integer Integer [Item] -- Prefix (Items Lenght) Items

type NBItemGroup = ItemGroup Integer Integer [Item]

data Batch a b c where
  Batch :: Tag -> Integer -> [NBItemGroup] -> Batch Tag Integer [NBItemGroup]

instance Show (ItemGroup a b c) where
  show (ItemGroup a b c) =
    "ItemGroup -> Prefix [" ++
    (show a) ++ "] Items Length [" ++ (show b) ++ "] Items " ++ (show c)

instance Show (Batch a b c) where
  show (Batch a b c) =
    "Batch -> Items Tag [" ++
    a ++ "] Length [" ++ (show b) ++ "] ItemsGroups " ++ (show c)
