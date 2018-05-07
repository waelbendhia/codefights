module DivingDeeper
  ( extractEachKth
  , firstDigit
  , differentSymbolsNaive
  , arrayMaxConsecutiveSum
  ) where

import Data.Char
import Data.List

extractEachKth :: [Int] -> Int -> [Int]
extractEachKth inputArray k
  | length inputArray <= k = taken
  | otherwise = taken ++ extractEachKth (drop k inputArray) k
  where
    taken = take (k - 1) inputArray

firstDigit :: [Char] -> Char
firstDigit = head . filter isDigit

differentSymbolsNaive :: String -> Int
differentSymbolsNaive = length . group . sort

arrayMaxConsecutiveSum :: (Num a, Ord a) => [a] -> Int -> a
arrayMaxConsecutiveSum inputArray k = maximum $ sumCons inputArray k

sumCons :: Num a => [a] -> Int -> [a]
sumCons l k = firstVal : (groupCons (firstVal, l) $ end)
  where
    (start, end) = splitAt k l
    firstVal = sum start
    groupCons _ [] = []
    groupCons (t, h:list) (f:xs) = t - h + f : groupCons (t - h + f, list) xs
      where
        newT = t - h + f
