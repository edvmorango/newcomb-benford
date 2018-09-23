{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CSVMain where

import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import NBLib

data OrderCSV = OrderCSV
  { amount :: Double
  } deriving (Show)

-- Must convert float to Integer on parsing
instance FromNamedRecord OrderCSV where
  parseNamedRecord r = OrderCSV <$> r .: "amount"

type OrderV = (Header, V.Vector OrderCSV)

csvReader :: IO (Either String OrderV)
csvReader =
  BL.readFile "dataset/orders.csv" >>= (\csv -> return (decodeByName csv)) --do

data CSVException =
  CSVException String
  deriving (Show)

instance Exception CSVException

-- csvMain :: IO ()
csvMain = do
  ordersV <- csvReader
  orders <-
    case ordersV of
      Left s -> throwIO $ CSVException s
      Right (_, v) -> return $ ((fmap (round . amount)) . V.toList) v
  return $ orders
  --putStrLn $ show (length orders)
