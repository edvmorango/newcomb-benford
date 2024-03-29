{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CSVMain where

import Charts
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Char
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
  (tag, items) <-
    case ordersV of
      Left s -> throwIO $ CSVException s
      Right (tagname, v) ->
        let tag = (C8.unpack . C8.fromStrict . V.head) tagname
            convert = round . amount
            tr = fmap (\e -> Item tag (convert e)) . V.toList
         in return $ (tag, tr v)
  points <- return $ batchPoints $ mkNBBatch tag items
  let ps = fmap (\(a, b) -> [realToFrac a, realToFrac b]) points
   in genChart "Orders" ps
