module ExploringTheWaters
  ( alternatingSums
  , addBorder
  , areSimilar
  , arrayChange
  , palindromeRearranging
  ) where

import Data.List

alternatingSums :: Num a => [a] -> [a]
alternatingSums [] = [0, 0]
alternatingSums [x1] = [x1, 0]
alternatingSums (x1:x2:xs) = [s1 + x1, s2 + x2]
  where
    [s1, s2] = alternatingSums xs

addBorder :: [String] -> [String]
addBorder picture = top : (map (\s -> '*' : s ++ "*") picture) ++ [top]
  where
    top = take (length (picture !! 0) + 2) $ repeat '*'

areSimilar :: (Num a, Eq a) => [a] -> [a] -> Bool
areSimilar a b = cond
  where
    comp = filter (\(x, y) -> x /= y) $ zip a b
    cond =
      length comp == 0 ||
      length comp == 2 &&
      let [(x1, y1), (x2, y2)] = comp
      in x1 == y2 && x2 == y1

arrayChange :: (Num a, Ord a) => [a] -> a
arrayChange [] = 0
arrayChange [_] = 0
arrayChange (x1:x2:xs) = dif + arrayChange (x2 + dif : xs)
  where
    dif = max (x1 - x2 + 1) 0

palindromeRearranging :: String -> Bool
palindromeRearranging inputString = numOdd <= 1
  where
    occurences = map length $ group $ sort inputString
    numOdd = length $ filter odd occurences
