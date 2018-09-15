module ChiSquared where

chiSquaredFormula :: Float -> Float -> Float
chiSquaredFormula e r = ((r - e) ** 2) / e

chiSquaredExecutor :: [(Float, Float)] -> [Float]
chiSquaredExecutor a = map (\(e, r) -> chiSquaredFormula e r) a

frequency :: [(Float, Float)] -> (Float, Float)
frequency t = (els, diff)
  where
    els = foldr (\(e, _) acc -> acc + e) 0 t
    diff = (-) 1 $ foldr (+) 0 (chiSquaredExecutor t)
