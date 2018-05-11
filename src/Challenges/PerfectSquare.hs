import Data.List

perfectSquareOrCube = length . intersect l . map read . permutations . show

l =
  1 : do
    x <- 2 : 3 : [5, 8] ++ [10 .. 30]
    [x ^ 2, x ^ 3]
