module NBLaw where

clog = logBase 10

logNumberMk :: Int -> Float
logNumberMk a = 1 + 1 / (fromIntegral a)

algarismLaw :: Int -> Float
algarismLaw v
  | v > 0 && v < 10 = (clog . logNumberMk) v
  | otherwise = 0
