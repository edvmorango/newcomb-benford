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

test :: [(Float, Float)]
test =
  [ (301, 302)
  , (176, 175)
  , (125, 126)
  , (97, 96)
  , (79, 80)
  , (67, 66)
  , (58, 59)
  , (51, 50)
  , (46, 46)
  ]
