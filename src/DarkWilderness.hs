module DarkWilderness
  ( growingPlant
  , knapsackLight
  , longestDigitsPrefix
  , digitDegree
  , bishopAndPawn
  ) where

import Data.Char
import Data.List

growingPlant :: (Integral a) => a -> a -> a -> a
growingPlant upSpeed downSpeed desiredHeight =
  (+ 1) $
  ceiling $
  fromIntegral (max 0 $ desiredHeight - upSpeed) /
  fromIntegral (upSpeed - downSpeed)

-- Is this perhaps a little overengineered? Yes, yes it is.
knapsackLight ::
     (Ord p2, Ord p1, Num p2, Num p1) => p2 -> p1 -> p2 -> p1 -> p1 -> p2
knapsackLight value1 weight1 value2 weight2 maxW = maxVal maxW items
  where
    items =
      sortBy
        (\(v1, _) (v2, _) -> compare v2 v1)
        [(value1, weight1), (value2, weight2)]
    maxVal maxW [] = 0
    maxVal maxW ((v1, w1):is) =
      if w1 <= maxW
        then v1 + maxVal (maxW - w1) is
        else maxVal maxW is

longestDigitsPrefix :: [Char] -> [Char]
longestDigitsPrefix = takeWhile isDigit

digitDegree :: Num p => Int -> p
digitDegree n
  | n < 10 = 0
  | otherwise = 1 + (digitDegree $ sum $ map digitToInt $ show n)

bishopAndPawn :: [Char] -> [Char] -> Bool
bishopAndPawn bishop pawn = abs (bx - px) == abs (by - py)
  where
    getVal [r, c] = (ord r - ord 'A', digitToInt c)
    (bx, by) = getVal bishop
    (px, py) = getVal pawn
