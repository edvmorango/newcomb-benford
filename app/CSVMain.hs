{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CSVMain where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import NBLib

csvMain :: IO ()
csvMain = do
  putStrLn "123123"
