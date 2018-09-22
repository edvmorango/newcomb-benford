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

-- taker :: Int -> Int -> [(Integer, Item)] -> [[Item]]  
mkBatchGroups :: Integer -> [(Integer, Item)] -> [NBItemGroup]
mkBatchGroups 10 _ = []
mkBatchGroups i v = (mkNBItemGroup i items) : mkBatchGroups (i + 1) t
  where
    (c, t) = span (\(a, _) -> a == i) v
    items = fmap (\(_, b) -> b) c

prepareGroups :: [Item] -> [NBItemGroup]
prepareGroups is = (mk . sort . fdr) is
  where
    prefix i =
      case (getPrefix (code i)) of
        Just p -> [(p, i)]
        _ -> []
    mk = mkBatchGroups 1
    sort = sortBy (\(a, _) (b, _) -> compare a b)
    fdr = foldr (\c acc -> (prefix c) ++ acc) []

mkNBBatch :: Tag -> [Item] -> NBBatch
mkNBBatch t is = Batch t len igs
  where
    filtered = filter (\i -> (tag i) == t) is
    igs = prepareGroups filtered
    len = foldr (\(ItemGroup _ l _) acc -> acc + l) 0 igs
