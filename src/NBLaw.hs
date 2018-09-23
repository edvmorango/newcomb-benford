module NBLaw where

clog = logBase 10

logNumberMk :: Integer -> Float
logNumberMk a = 1 + 1 / (fromIntegral a)

algarismLaw :: Integer -> Maybe Float
algarismLaw v
  | v > 0 && v < 10 = Just $ (clog . logNumberMk) v
  | otherwise = Nothing

algarismLawUnsafe :: Integer -> Float
algarismLawUnsafe v = (clog . logNumberMk) v
