module Charts where

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

barTitle = ["Expected Frequency", "Real Frequency"]

genChart :: String -> [[Double]] -> IO ()
genChart title points =
  toFile def ("charts/" ++ title ++ ".png") $ do
    layout_title .= title
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst p)
    plot $ fmap plotBars $ bars barTitle (addIndexes (map snd p))
  where
    p = zip (fmap (show) [1 .. 9]) points
