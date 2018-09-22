{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module NBLib where

import Data.Char
import Data.List
import Data.Maybe
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

type NBBatch = Batch Tag Integer [NBItemGroup]

------ ItemGroup Instances
instance Show (ItemGroup a b c) where
  show (ItemGroup a b c) =
    "ItemGroup -> Prefix [" ++
    (show a) ++ "] Items Length [" ++ (show b) ++ "] Items " ++ (show c)

instance Semigroup (ItemGroup a b c) where
  (ItemGroup p l is) <> (ItemGroup _ l' is') = ItemGroup p (l + l') (is ++ is')

------ Batch Instances
instance Show (Batch a b c) where
  show (Batch a b c) =
    "Batch -> Items Tag [" ++
    a ++ "] Length [" ++ (show b) ++ "] ItemsGroups " ++ (show c)

instance Semigroup (Batch a b c) where
  (Batch t l igs) <> (Batch _ l' igs') = Batch t (l + l') merge
    where
      merge = fmap (\(a, b) -> a <> b) (zip igs igs')

------ Constructors
mkNBItemGroup :: Integer -> [Item] -> NBItemGroup
mkNBItemGroup p is = ItemGroup p l is
  where
    l = (toInteger . length) is

-- First Digit
getPrefix :: Integer -> Maybe Integer
getPrefix n
  | n <= 0 = Nothing
  | otherwise = Just ((toInteger . digitToInt . head . show) n)

-- gb :: [Item] -> [[Item]]
-- gb :: [Item] -> [(Integer, Item)]
adjustGroup :: Integer -> [[Item]] -> [[Item]] -> [[Item]]
adjustGroup 0 acc _ = acc
adjustGroup i acc m@(hm:tm) =
  if (dec hm) == (10 - i)
    then inc (acc ++ [hm]) tm
    else inc (acc ++ [[]]) m
  where
    dec :: [Item] -> Integer
    dec [] = 0
    dec (h:_) = (fromJust . getPrefix . code) h
    inc = adjustGroup (i - 1)

gb its = ((adjustGroup 9 [[]]) . mp . grp . sort . fdr) its
  where
    prefix i =
      case (getPrefix (code i)) of
        Just p -> [(p, i)]
        _ -> []
    mp = (fmap . fmap) (\(_, e) -> e)
    grp = groupBy (\(a, _) (b, _) -> (==) a b)
    sort = sortBy (\(a, _) (b, _) -> compare a b)
    fdr = foldr (\c acc -> (prefix c) ++ acc) []
