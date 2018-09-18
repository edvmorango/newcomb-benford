module NBLaw where

clog = logBase 10

logNumberMk :: Int -> Float
logNumberMk a = 1 + 1 / (fromIntegral a)

algarismLaw :: Int -> Maybe Float
algarismLaw v
  | v > 0 && v < 10 = Just $ (clog . logNumberMk) v
  | otherwise = Nothing
