module SmoothSailing
  ( allLongestStrings
  , commonCharacterCount
  , isLucky
  , sortByHeight
  , reverseParentheses
  ) where

import Data.List

allLongestStrings :: [String] -> [String]
allLongestStrings inputArray = filter ((== maxLen) . length) inputArray
  where
    maxLen = maximum $ map length inputArray

commonCharacterCount :: String -> String -> Int
commonCharacterCount s1 s2 =
  snd $
  foldl
    (\(p, t) c ->
       case removeFrom c p of
         Just l -> (l, t + 1)
         Nothing -> (p, t))
    (s1, 0)
    s2

removeFrom :: Eq p => p -> [p] -> Maybe [p]
removeFrom c s = f [] s
  where
    f _ [] = Nothing
    f xs (y:ys) =
      if y == c
        then Just (xs ++ ys)
        else f (y : xs) ys

isLucky :: Int -> Bool
isLucky n = even lenN && f firstHalf == f secondHalf
  where
    strN = show n
    lenN = length strN
    (firstHalf, secondHalf) =
      (take (lenN `div` 2) strN, drop (lenN `div` 2) strN)
    f = sum . map digitToInt

digitToInt :: Num p => Char -> p
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9
digitToInt c = error (c : " is not a digit")

sortByHeight :: [Int] -> [Int]
sortByHeight a = insertIntoTreesWithInterval trees $ sort nums
  where
    trees = treesWithInterval a
    nums = filter (/= -1) a

treesWithInterval :: [Int] -> [Int]
treesWithInterval a =
  group a >>= \x ->
    if head x == -1
      then x
      else [length x]

insertIntoTreesWithInterval :: [Int] -> [Int] -> [Int]
insertIntoTreesWithInterval ts [] = ts
insertIntoTreesWithInterval [] _ = []
insertIntoTreesWithInterval (t:ts) as
  | t == -1 = t : insertIntoTreesWithInterval ts as
  | otherwise = take t as ++ insertIntoTreesWithInterval ts (drop t as)

reverseParentheses :: [Char] -> [Char]
reverseParentheses [] = []
reverseParentheses ('(':cs) = reverseParentheses $ inParens [] cs
reverseParentheses (c:cs) = c : reverseParentheses cs

inParens :: [Char] -> [Char] -> [Char]
inParens acc [] = acc
inParens acc ('(':cs) = inParens acc $ inParens [] cs
inParens acc (')':cs) = acc ++ cs
inParens acc (c:cs) = inParens (c : acc) cs
