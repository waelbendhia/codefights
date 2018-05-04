module EdgeOfTheOcean
  ( adjacentElementsProduct
  , makeArrayConsecutive2
  , shapeArea
  , almostIncreasingSequence
  , matrixElementsSum
  ) where

import Data.List

adjacentElementsProduct :: [Integer] -> Integer
adjacentElementsProduct = maximum . prods
  where
    prods (x1:x2:xs) = (x1 * x2) : (prods $ x2 : xs)
    prods [] = []
    prods [x] = []

shapeArea :: (Eq p, Num p) => p -> p
shapeArea 0 = 0
shapeArea 1 = 1
shapeArea n = (shapeArea (n - 1)) + (n - 1) * 4

makeArrayConsecutive2 :: (Num p, Ord p) => [p] -> p
makeArrayConsecutive2 [] = 0
makeArrayConsecutive2 statues =
  fst $ foldl (\(total, prev) x -> (total + x - prev - 1, x)) (0, head l - 1) l
  where
    l = sort statues

-- Probably a bunch of redundancies here but whatever
almostIncreasingSequence :: Ord a => [a] -> Bool
almostIncreasingSequence seq = almostIncreasing seq || increasing (tail seq)

almostIncreasing :: Ord a => [a] -> Bool
almostIncreasing [_, _] = True
almostIncreasing [x1, x2, x3] = increasing [x1, x3] || increasing [x1, x2]
almostIncreasing [x1, x2, x3, x4] =
  increasing [x1, x2, x4] || increasing [x1, x3, x4] || increasing [x1, x2, x3]
almostIncreasing (x1:x2:x3:x4:xs)
  | increasing [x1, x2] = almostIncreasing (x2 : x3 : x4 : xs)
  | otherwise =
    (increasing [x1, x2, x4] || increasing [x1, x3, x4]) && increasing (x4 : xs)

increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x1:x2:xs) = x1 < x2 && increasing (x2 : xs)

matrixElementsSum :: [[Integer]] -> Integer
matrixElementsSum matrix = sum $ map fst $ mergeMatrix matrix

mergeRooms :: [(Integer, Integer)] -> [Integer] -> [(Integer, Integer)]
mergeRooms =
  zipWith
    (\(t, a) b ->
       if a == 0
         then (t, 0)
         else (t + b, b))

mergeMatrix :: [[Integer]] -> [(Integer, Integer)]
mergeMatrix [] = []
mergeMatrix ls = foldl mergeRooms (zip (head ls) $ head ls) $ tail ls
