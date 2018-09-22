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

instance Show (ItemGroup a b c) where
  show (ItemGroup a b c) =
    "ItemGroup -> Prefix [" ++
    (show a) ++ "] Items Length [" ++ (show b) ++ "] Items " ++ (show c)
