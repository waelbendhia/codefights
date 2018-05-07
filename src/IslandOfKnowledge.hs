module IslandOfKnowledge
  ( areEquallyStrong
  , arrayMaximalAdjacentDifference
  , isIPv4Address
  , avoidObstacles
  ) where

import Text.Read

areEquallyStrong :: Int -> Int -> Int -> Int -> Bool
areEquallyStrong yourLeft yourRight friendsLeft friendsRight =
  (yourLeft == friendsLeft && yourRight == friendsRight) ||
  (yourRight == friendsLeft && yourLeft == friendsRight)

arrayMaximalAdjacentDifference :: [Integer] -> Integer
arrayMaximalAdjacentDifference = maximum . difs
  where
    difs [] = []
    difs [x] = []
    difs (x1:x2:xs) = abs (x2 - x1) : difs (x2 : xs)

isIPv4Address :: [Char] -> Bool
isIPv4Address inputString =
  length nums == 4 &&
  all
    (\x ->
       case x of
         Just y -> y <= 255 && y >= 0
         Nothing -> False)
    nums
  where
    nums :: [Maybe Int]
    nums = map readMaybe $ splitOn '.' inputString

splitOn :: Eq p => p -> [p] -> [[p]]
splitOn y s = f [] [] s
  where
    f t acc (c:cs)
      | c == y = f (t ++ [acc]) [] cs
      | otherwise = f t (acc ++ [c]) cs
    f t acc [] = (t ++ [acc])

avoidObstacles :: Integral a => [a] -> a
avoidObstacles inputArray = head $ filter checker $ [1 ..]
  where
    checker n = all (\o -> o `rem` n /= 0) inputArray
