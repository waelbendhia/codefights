module IslandOfKnowledge
  ( areEquallyStrong
  , arrayMaximalAdjacentDifference
  , isIPv4Address
  , avoidObstacles
  , boxBlur
  , minesweeper
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

boxBlur :: (Integral a, Integral b) => [[a]] -> [[b]]
boxBlur ls =
  map (map truncate) $ map blurLine $ blurLines $ map (map fromIntegral) ls

blurLine :: Fractional a => [a] -> [a]
blurLine [] = []
blurLine [_] = []
blurLine [_, _] = []
blurLine (x1:x2:x3:xs) = (x1 + x2 + x3) / 3 : blurLine (x2 : x3 : xs)

blurLines :: Fractional d => [[d]] -> [[d]]
blurLines [] = []
blurLines [_] = []
blurLines [_, _] = []
blurLines (l1:l2:l3:ls) =
  zipWith3 (\x1 x2 x3 -> (x1 + x2 + x3) / 3) l1 l2 l3 : blurLines (l2 : l3 : ls)

-- Not very FP and slow but hey it works
minesweeper :: Num b => [[Bool]] -> [[b]]
minesweeper [] = []
minesweeper matrix = do
  i <- [0 .. h - 1]
  let l = do
        j <- [0 .. w - 1]
        return $
          sum $
          map
            (uncurry get)
            [ (i - 1, j - 1)
            , (i - 1, j)
            , (i - 1, j + 1)
            , (i, j - 1)
            , (i, j + 1)
            , (i + 1, j - 1)
            , (i + 1, j)
            , (i + 1, j + 1)
            ]
  return l
  where
    (h, w) = (length matrix, length $ matrix !! 0)
    get i j =
      if i >= 0 && i <= h - 1 && j >= 0 && j <= w - 1 && (matrix !! i) !! j
        then 1
        else 0
